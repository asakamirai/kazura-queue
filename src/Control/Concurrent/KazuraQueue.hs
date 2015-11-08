{-# LANGUAGE TypeFamilies #-}

-- | KazuraQueue is the fast queue implementation inspired by unagi-chan.

module Control.Concurrent.KazuraQueue
    ( Queue
    , newQueue
    , readQueue
    , readQueueWithoutMask
--    , tryReadQueue
    , writeQueue
    , writeQueueWithoutMask
--    , tryWriteQueue
    , lengthQueue
    , lengthQueue'
--    , skipReadQueue
    ) where

import           Control.Concurrent.WVar (WCached, WVar)
import qualified Control.Concurrent.WVar as WVar

import qualified Control.Concurrent      as CC
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception       as E
import qualified Control.Monad           as M
import           Control.Monad.Primitive (RealWorld)

import qualified Data.Atomics         as Atm
import qualified Data.Atomics.Counter as Atm
import           Data.Bits            ((.&.))
import qualified Data.Bits            as Bits
import           Data.IORef           (IORef)
import qualified Data.IORef           as Ref
import qualified Data.Primitive.Array as Arr

--------------------------------
-- constants and its utilities

{-# INLINE bufferLength #-}
bufferLength :: Int
bufferLength = 64

{-# INLINE logBufferLength #-}
logBufferLength :: Int
logBufferLength = 6

{-# INLINE divModBufferLength #-}
divModBufferLength :: Int -> (Int,Int)
divModBufferLength n = d `seq` m `seq` (d,m)
    where
        d = n `Bits.unsafeShiftR` logBufferLength
        m = n .&. (bufferLength - 1)

--------------------------------
-- Queue

-- | Type of a Queue. /a/ is the type of an item in the Queue.
data Queue a = Queue
    { queueWriteStream  :: {-# UNPACK #-} !(IORef (Stream a))
    , queueWriteCounter :: {-# UNPACK #-} !Atm.AtomicCounter
    , queueReadStream   :: {-# UNPACK #-} !(IORef (Stream a))
    , queueReadState    :: {-# UNPACK #-} !(WVar (ReadState a))
    , queueNoneTicket   ::                !(Atm.Ticket (Item a))
    }

data ReadState a = ReadState
    { rsCounter :: {-# UNPACK #-} !Atm.AtomicCounter
    , rsLimit   :: {-# UNPACK #-} !StreamIndex
    }

type Buffer a = Arr.MutableArray RealWorld (Item a)

type BufferSource a = IO (Buffer a)

data Item a =
      Item a
    | None
    | Wait {-# UNPACK #-} !(MVar a)
    | Done

data Stream a = Stream
    { streamBuffer :: {-# UNPACK #-} !(Buffer a)
    , streamNext   :: {-# UNPACK #-} !(IORef (NextStream a))
    , streamOffset :: {-# UNPACK #-} !StreamIndex
    }

data NextStream a =
      NextStream {-# UNPACK #-} !(Stream a)
    | NextSource                !(BufferSource a)

type StreamIndex = Int
type BufferIndex = Int

------------------------------

newBufferSource :: IO (BufferSource a)
newBufferSource = do
    arr <- Arr.newArray bufferLength None
    return (Arr.cloneMutableArray arr 0 bufferLength)

newReadState :: IO (WVar (ReadState a))
newReadState = do
    rcounter <- Atm.newCounter $ -1
    WVar.newWVar ReadState
        { rsCounter = rcounter
        , rsLimit   = -1
        }

-- | Create a new empty 'Queue'.
newQueue :: IO (Queue a)
newQueue = do
    bufSrc     <- newBufferSource
    buf        <- bufSrc
    noneTicket <- Atm.readArrayElem buf 0
    next       <- Ref.newIORef $ NextSource bufSrc
    let stream = Stream buf next 0
    wstream <- Ref.newIORef stream
    wcounter <- Atm.newCounter $ -1
    rstream <- Ref.newIORef stream
    rsvar <- newReadState
    return Queue
        { queueWriteStream  = wstream
        , queueWriteCounter = wcounter
        , queueReadStream   = rstream
        , queueReadState    = rsvar
        , queueNoneTicket   = noneTicket
        }

----------------------------------------------------------

{-# INLINE waitItem #-}
waitItem :: Buffer a -> BufferIndex -> IO ()
waitItem buf bufIdx = do
    ticket <- Atm.readArrayElem buf bufIdx
    case Atm.peekTicket ticket of
        None    -> do
            mv <- MVar.newEmptyMVar
            (_ret, next) <- Atm.casArrayElem buf bufIdx ticket $! Wait mv
            case Atm.peekTicket next of
                None     -> error "impossible case waitItem"
                Wait mv' -> M.void $ MVar.readMVar mv'
                _        -> return ()
        Wait mv -> M.void $ MVar.readMVar mv
        _       -> return ()

{-# INLINE writeItem #-}
writeItem :: Buffer a -> BufferIndex -> Atm.Ticket (Item a) -> a -> IO ()
writeItem buf bufIdx ticket a = do
    (suc, next) <- Atm.casArrayElem buf bufIdx ticket (Item a)
    M.unless suc $ case Atm.peekTicket next of
        Wait mv -> do
            Arr.writeArray buf bufIdx $ Item a
            MVar.putMVar mv a
        _ -> error "impossible case writeItem"

----------------------------------------------------------

-- | Read an item from the 'Queue'.
readQueue :: Queue a -> IO a
readQueue = E.mask_ . readQueueRaw

-- | Non-masked version of 'readQueue'.
--   It is not safe for asynchronous exception.
readQueueWithoutMask :: Queue a -> IO a
readQueueWithoutMask = readQueueRaw

{-# INLINE readQueueRaw #-}
readQueueRaw :: Queue a -> IO a
readQueueRaw queue@(Queue _ _ _ rsvar _) =
    WVar.cacheWVar rsvar >>= readStream queue

{-# INLINE readStream #-}
readStream :: Queue a -> WCached (ReadState a) -> IO a
readStream queue rswc0 = do
    rstr0 <- Ref.readIORef rstrRef
    strIdx <- Atm.incrCounter 1 rcounter
    if strIdx - rlimit0 > 0
        then do
            rswt1 <- extendReadStreamWithLock rstr0
            readStream queue rswc0 { WVar.cachedTicket = rswt1 }
        else do
            (bufIdx, rstr1) <- targetStream rstr0 strIdx
            let buf = streamBuffer rstr1
            M.when (bufIdx == 0) $ Ref.writeIORef rstrRef rstr1
            item <- Arr.readArray buf bufIdx
            Arr.writeArray buf bufIdx Done
            case item of
                Item a -> return a
                _      -> error "impossible case readQueue"
    where
        rswt0 = WVar.cachedTicket rswc0
        rstrRef = queueReadStream queue
        (ReadState rcounter rlimit0) = WVar.readWTicket rswt0
        extendReadStreamWithLock rstr0 = do
            (suc, rswt1) <- WVar.tryTakeWCached rswc0
            let rstate1 = WVar.readWTicket rswt1
            if suc
                then do
                    rstate2 <- extendReadStream rstate1 rstr0
                        `E.onException` WVar.putWCached rswc0 rstate1
                    WVar.putWCached rswc0 rstate2
                else WVar.readFreshWCached rswc0 { WVar.cachedTicket = rswt1 }

{-# INLINE extendReadStream #-}
extendReadStream :: ReadState a -> Stream a -> IO (ReadState a)
extendReadStream rstate0 rstr = do
    let rlimit0 = rsLimit rstate0
    rlimit1 <- searchOrWaitStream rstr $ rlimit0 + 1
    rcounter1 <- Atm.newCounter rlimit0
    return rstate0
            { rsCounter = rcounter1
            , rsLimit   = rlimit1
            }
    where
        {-# INLINE searchOrWaitStream #-}
        searchOrWaitStream rstr0 rlimit0p = do
            (rlimit1, rstr1) <- searchStreamReadLimit rstr0 rlimit0p
            M.when (rlimit0p == rlimit1) $ do
                let (Stream buf1 _ offset1) = rstr1
                    bufIdx1 = rlimit1 - offset1
                waitItem buf1 bufIdx1
            return $! rlimit1 - 1

-- | Write an item to the 'Queue'.
-- The item is evaluated (WHNF) before actual queueing.
writeQueue :: Queue a -> a -> IO ()
writeQueue queue = E.mask_ . writeQueueRaw queue

-- | Non-masked version of 'writeQueue'.
--   It is not safe for asynchronous exception.
writeQueueWithoutMask :: Queue a -> a -> IO ()
writeQueueWithoutMask = writeQueueRaw

{-# INLINE writeQueueRaw #-}
writeQueueRaw :: Queue a -> a -> IO ()
writeQueueRaw (Queue wstrRef wcounter _ _ noneTicket) a = do
    wstr0 <- Ref.readIORef wstrRef
    strIdx <- Atm.incrCounter 1 wcounter
    (bufIdx, wstr1) <- targetStream wstr0 strIdx
    writeItem (streamBuffer wstr1) bufIdx noneTicket a
    M.when (bufIdx == 0) $ Ref.writeIORef wstrRef wstr1

{-# INLINE targetStream #-}
targetStream :: Stream a -> StreamIndex -> IO (BufferIndex, Stream a)
targetStream str0@(Stream _ _ offset) strIdx = do
    let (strNum, bufIdx) = divModBufferLength $ strIdx - offset
    str1 <- getStream strNum bufIdx str0
    return (bufIdx, str1)
    where
        {-# INLINE getStream #-}
        getStream 0 _      strA = return strA
        getStream n bufIdx strA = do
            strB <- waitNextStream strA bufIdx
            getStream (n-1) bufIdx strB

{-# NOINLINE waitNextStream #-}
waitNextStream :: Stream a -> Int -> IO (Stream a)
waitNextStream (Stream _ nextStrRef offset) = go
    where
        {-# INLINE go #-}
        go wait = do
            ticket <- Atm.readForCAS nextStrRef
            case Atm.peekTicket ticket of
                NextStream strNext -> return strNext
                nextSrc@(NextSource bufSrc)
                    | wait > 0  -> do
                        CC.yield
                        go (wait - 1)
                    | otherwise -> do
                        newBuf <- bufSrc
                        newNext <- Ref.newIORef nextSrc
                        let nextStrCand = NextStream Stream
                                { streamBuffer = newBuf
                                , streamNext   = newNext
                                , streamOffset = offset + bufferLength
                                }
                        (_, next) <- Atm.casIORef nextStrRef ticket nextStrCand
                        case Atm.peekTicket next of
                            NextStream nextStr -> return nextStr
                            NextSource _ -> go 1

-- | Search 'Stream' and return 'StreamIndex' and its 'Stream'
--     of the oldest not-available Item.
{-# INLINE searchStreamReadLimit #-}
searchStreamReadLimit :: Stream a -> StreamIndex -> IO (StreamIndex, Stream a)
searchStreamReadLimit baseStr strIdx =
    go (strIdx - streamOffset baseStr) baseStr
    where
        {-# INLINE go #-}
        go bufIdx stream@(Stream buf _ offset) = do
            ret <- searchBufferReadLimit buf bufIdx
            case ret of
                Just retBufIdx -> return (offset + retBufIdx, stream)
                Nothing -> waitNextStream stream 0 >>= go 0

-- | Search 'Buffer' and return 'BufferIndex'
--     of the oldest not-available Item.
--   If all Item in the Buffer is ready, return Nothing.
{-# INLINE searchBufferReadLimit #-}
searchBufferReadLimit :: Buffer a -> BufferIndex -> IO (Maybe BufferIndex)
searchBufferReadLimit buf = go
    where
        {-# INLINE go #-}
        go bufIdx
            | idxIsOutOfBuf = return Nothing
            | otherwise = do
                item <- Arr.readArray buf bufIdx
                case item of
                    None   -> return $ Just bufIdx
                    Wait _ -> return $ Just bufIdx
                    _      -> go $ bufIdx + 1
            where
                idxIsOutOfBuf = bufIdx >= bufferLength

-- | Get the length of the items in the 'Queue'.
-- Caution: It returns minus value
--     when the Queue is empty and waiting for new value.
lengthQueue :: Queue a -> IO Int
lengthQueue (Queue _ wcounter _ rsvar _) = do
    rs <- WVar.readWVar rsvar
    rcount <- Atm.readCounter $ rsCounter rs
    wcount <- Atm.readCounter wcounter
    return $ wcount - rcount

-- | Non-minus version of 'lengthQueue'
lengthQueue' :: Queue a -> IO Int
lengthQueue' queue = f <$> lengthQueue queue
    where
        f i | i > 0     = i
            | otherwise = 0


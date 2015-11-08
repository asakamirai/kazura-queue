module Main where

import Test.DocTest

main :: IO ()
main = doctest
    [ "-isrc"
    , "src/Control/Concurrent/WVar.hs"
    , "src/Control/Concurrent/KazuraQueue.hs"
    ]

#!/bin/sh

rm -rf export
git checkout-index -a -f --prefix=export/

rm -f export/hackage.sh

cd export
stack upload .


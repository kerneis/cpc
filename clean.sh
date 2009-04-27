#!/bin/sh

set -e

make distclean
cd runtime
make clean
cd ../samples
make clean
cd ..

#!/bin/sh

set -e

# For people using ocamlbuild (cf. build.sh)
# rm -rf _build bin/cpc.asm.exe doc/html

# For other people
make distclean

cd runtime
make clean
cd ../samples
make clean
cd ..

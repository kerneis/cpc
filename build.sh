#!/bin/sh

set -e

# If you want to use ocamlbuild:
# make BUILDER=ocamlbuild
# Otherwise:
make
cd runtime
make clean all
cd ../samples
make clean all
cd ..

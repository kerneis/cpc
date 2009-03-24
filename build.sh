#!/bin/sh

set -o nounset
set -e

sh configure
make clean
ocamlbuild -Xs runtime,samples cil.otarget
cp _build/src/main.native obj/x86_LINUX/cilly.asm.exe
cd runtime
make clean
make
cd ../samples
make clean
make
cd ..

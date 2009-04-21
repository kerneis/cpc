#!/bin/sh

set -e

# Adventurous people daring to try ocamlbuild might use the following
# lines. It worked pretty well for me but was a PITA to maintain in
# parallel with CIL build system.
# Warning: this is not compatible with make, so if you try to switch
# from one to the another, ocamlbuild will ask you to run a sanitize
# script. You might also have to remove doc/html.

#ocamlbuild -Xs runtime,samples cil.otarget
#cp _build/src/main.native bin/cpc.asm.exe
#ln -sfn ../_build/doc/cil.docdir doc/html

# Most people should use this (not so insane) default instead
make NATIVECAML=1

# The following does not depend on the choice made above
cd runtime
make clean all
cd ../samples
make clean all
cd ..

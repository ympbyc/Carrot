#!/bin/bash

version="2.2.0"

echo "Installing Carrot..."

# cp bin/carrot-repl.scm    /usr/local/bin/carrot-repl
cp bin/carrot-read.scm    /usr/local/bin/carrot-read
cp bin/carrot-type.scm    /usr/local/bin/carrot-type
cp bin/carrot-compile.scm /usr/local/bin/carrot-compile
cp bin/carrot-vm.scm      /usr/local/bin/carrot-vm

chmod u+x /usr/local/bin/carrot-*

mkdir /usr/local/share/Carrot
mkdir /usr/local/share/Carrot/$version
mkdir /usr/local/share/Carrot/$version/lib
mkdir /usr/local/share/Carrot/$version/compilers
mkdir /usr/local/share/Carrot/$version/examples

cp lib/*        /usr/local/share/Carrot/$version/lib/
cp compilers/*  /usr/local/share/Carrot/$version/compilers/
cp examples/*   /usr/local/share/Carrot/$version/examples/

echo "Installation complete."
echo ""
echo "Example Usage:"
#echo "carrot-repl"
echo "    cat foo.carrot bar.carrot | carrot-read | carrot-compile to-js > main.js"
echo "    cat foo.carrot | carrot-read | carrot-compile to-carrot-vm | carrot-vm"
echo ""
echo "Enjoy!"
echo ""

#!/bin/zsh

if [ ! -d ./obj ]; then 
    mkdir obj
fi

if [ ! -d ./lib ]; then 
    mkdir lib
fi

pushd lib
     rm -rf *
popd

pushd obj
     rm -rf *
popd

gprbuild -g --no-complete-output -Pvst3

if [ -e ./lib/libsami.so ]; then 
    cp ./lib/libsami.so ./lib/sami.vst3
fi

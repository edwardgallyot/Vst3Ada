#!/bin/zsh

if [ ! -d ./obj ]; then 
    mkdir obj
fi

if [ ! -d ./lib ]; then 
    mkdir lib
fi

if [ -d ./lib ]; then 
    pushd lib
         rm -rf *
    popd
fi

if [ -d ./obj ]; then 
    pushd obj
        rm -rf *
    popd
fi

gprbuild -g -gnat2022 --no-complete-output -Pvst3

if [ -e ./lib/libsami.so ]; then 
    cp ./lib/libsami.so ./lib/sami.vst3
fi

#!/bin/zsh

# alr clean
alr build

pushd lib
    if [ -d sami.vst3 ]; then 
        rm -rf sami.vst3
    fi
    
    if [ -e libsami.so ]; then 
        mkdir -p sami.vst3/Contents/x86_64-linux
        cp libsami.so sami.vst3/Contents/x86_64-linux/sami.so
    fi
popd


#!/bin/zsh

# alr clean
alr build

if [ -e ./lib/libsami.so ]; then 
    cp ./lib/libsami.so ./lib/sami.vst3
fi


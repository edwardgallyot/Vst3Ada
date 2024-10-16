#!/bin/zsh

# alr clean
alr build

echo Post Build Started.
pushd lib
    if [ -d sami.vst3 ]; then 
        echo Cleaning Bundle
        rm -rf sami.vst3
    fi
    
    if [ -e libsami.so ]; then 
        echo Copying Lib to Bundle
        mkdir -p sami.vst3/Contents/x86_64-linux
        cp libsami.so sami.vst3/Contents/x86_64-linux/sami.so
    fi

    if [ ! -d $HOME/.vst3 ]; then
        mkdir $HOME/.vst3
    fi

    if [ -d sami.vst3 ]; then 
        echo Copying Bundle to System Folders 
        cp -r sami.vst3 $HOME/.vst3
    fi

popd
echo Post Build Completed.


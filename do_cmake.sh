#!/bin/sh

TARGET=`uname -s`
TARGET=${TARGET}_`uname -m`

echo "Creating $TARGET"
mkdir -p build_$TARGET
cd build_$TARGET
cmake ..
cmake --build . --config RelWithDebInfo --target install

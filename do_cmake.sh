#!/bin/sh

TARGET=`uname -s`
TARGET=${TARGET}_`uname -m`

echo "Creating $TARGET"
mkdir -p c_src/$TARGET
cd c_src/$TARGET
cmake ..
cmake --build . --config RelWithDebInfo --target install

#!/usr/bin/env bash
set -e

esy install
esy build
esy release bin
cd _release
cd *
npm pack
mv reason-wall-demo-* ../../
cd ../../
npm install -g ./reason-wall-demo-*
which ReasonWallDemo.exe
npm remove -g reason-wall-demo

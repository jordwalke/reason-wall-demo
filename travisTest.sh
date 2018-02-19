#!/usr/bin/env bash
set -e

esy install
esy build
esy x which ReasonWallGallery.exe
esy x which ReasonWallUi.exe
esy release bin
npm install -g ./_release
# Now we don't need the esy x prefix
which ReasonWallGallery.exe
which ReasonWallUi.exe
npm remove -g reason-wall-demo

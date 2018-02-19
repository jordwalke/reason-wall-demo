#!/usr/bin/env bash
set -e
npm remove -g esy
# Test esy preview branch - make the release with it.
npm install -g esy@preview
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

# Now test the latest branch of esy
npm remove -g esy
npm install -g esy@latest
esy install
esy build
esy x which ReasonWallGallery.exe
esy x which ReasonWallUi.exe


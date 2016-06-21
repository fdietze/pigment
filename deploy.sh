#!/bin/bash
NAME="pigment"

# sbt fullOptJS

mkdir -p deploy
cp $NAME.html deploy
cp target/scala-2.11/$NAME-{jsdeps.min.js,opt.js,launcher.js} deploy

# ipfs add -r deploy
# ipfs name publish /ipfs/$DIRHASH $PUBKEY

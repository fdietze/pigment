#!/bin/bash
NAME="pigment"

sbt fullOptJS

mkdir -p deploy
cp $NAME.html deploy
cp target/scala-2.11/$NAME-{jsdeps.min.js,opt.js,launcher.js} deploy

DIRHASH=$(ipfs add -r deploy | tail -1 | cut -d " " -f2)
ipfs name publish /ipfs/$DIRHASH

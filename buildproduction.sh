#!/bin/bash -e

NAME="pigment"
OUT="out"

sbt fullOptJS

rm -r $OUT
mkdir -p $OUT
cp index.html $OUT
cp target/scala-2.11/$NAME-{jsdeps.min.js,opt.js,launcher.js} $OUT

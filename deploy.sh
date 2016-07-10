#!/bin/bash -e
NAME="pigment"
OUT="out"

sbt fullOptJS

rm -r $OUT
mkdir -p $OUT
cp index.html $OUT
cp target/scala-2.11/$NAME-{jsdeps.min.js,opt.js,launcher.js} $OUT

DIRHASH=$(ipfs add -r $OUT | tail -1 | cut -d " " -f2)
# ipfs name publish /ipfs/$DIRHASH

URL="https://ipfs.io/ipfs/$DIRHASH"
echo $URL

ssh neptun ipfs pin add /ipfs/$DIRHASH
git tag master-ipfs -f -m "$URL"
git push --tags --force


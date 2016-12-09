#!/bin/bash -e
NAME="pigment"
OUT="out"

./buildproduction.sh

BRANCH=$(git rev-parse --abbrev-ref HEAD)

git stash
git checkout gh-pages
mv $OUT/* .
rm -r $OUT
git add *.html *.js
git commit --amend --no-edit
git push --force
git checkout $BRANCH
git stash pop

#!/usr/bin/env sh

gittest="$(pwd)/gittest"
rm -rf $gittest
mkdir -p $gittest

mkdir -p $gittest/remote
cd $gittest/remote && (
    git init --bare
)

cd $gittest && (
    git clone remote has-commit
    echo contents > has-commit/file1 
)
cd $gittest/has-commit && (
    git add -A
    git commit -am "Initial commit for test repo"
    git push
)

cd $gittest && (
    git clone remote dirty-unchecked
    echo unchecked > $gittest/dirty-unchecked/unchecked
)

cd $gittest && (
    git clone remote dirty-checked
)
cd $gittest/dirty-checked && (
    echo checked > checked
    git add checked
)
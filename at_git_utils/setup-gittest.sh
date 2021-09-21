#!/usr/bin/env bash

gittest="$(pwd)/gittest"
echo "Setting up git repos in $gittest"
rm -rf $gittest
mkdir -p $gittest

setup_remote() {
    mkdir -p "$gittest/remote"
    cd "$gittest/remote" && (
        git init --bare
    )

    setup=$(mktemp -d)
    git clone "$gittest/remote" "$setup"
    cd "$setup" && (
        git checkout -b basebr
        echo contents > file1 
        git add -A
        git commit -m "Initial commit for test repo"

        echo second contents > file2
        git add -A
        git commit -m "Second commit for test repo"
        git push -u origin basebr
    )
    rm -rf "$setup"
    cd $gittest
}

setup_remote_wbrexists() {
    setup=$(mktemp -d)
    git clone --bare "$gittest/remote" "$gittest/remote-wbrexists"
    git clone "$gittest/remote-wbrexists" "$setup"
    cd "$gittest/remote-wbrexists" && (
        echo "third contents" > file3
        git checkout -b workbr
        git add -A
        git commit -m "Commit off of working branch"
        git push -u origin workbr
    )
    rm -rf "$setup"
    cd $gittest
}

setup_dirty_unchecked() {
    git clone "$gittest/remote" "$gittest/dirty-unchecked"
    cd "$gittest/dirty-unchecked" && (
        git checkout basebr
        echo unchecked > unchecked
    )
}

setup_dirty_checked() {
    git clone "$gittest/remote" "$gittest/dirty-checked"
    cd "$gittest/dirty-checked" && (
        git checkout basebr
        echo checked > checked
        git add checked
    )
}

setup_remote
setup_remote_wbrexists
setup_dirty_checked
setup_dirty_unchecked

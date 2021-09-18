#!/usr/bin/env sh

REMOTE=$1
WORK_BRANCH=$2
MAIN_BRANCH=$3

# Clean the directory
git restore :/

# Ensure that origin points to the remote so we can fetch the data
git remote add origin $REMOTE || git remote set-url origin $REMOTE
git fetch 

# Create work branch at the main branch
git checkout origin/$MAIN_BRANCH
git checkout -b $WORK_BRANCH

#!/usr/bin/env sh

REMOTE=$1
WORK_BRANCH=$2
MAIN_BRANCH=$3

# Ensure git repository exists
git init

# Clean the directory
git add -A
git reset --hard

# Ensure that origin points to the remote so we can fetch the data
git remote add origin $REMOTE || git remote set-url origin $REMOTE

# Fetch the work branch, or the main branch as a fallback
git fetch origin --depth 1 $WORK_BRANCH || git fetch origin --depth 1 $MAIN_BRANCH

# Delete the cached origin tags if needed
git remote prune origin  

# Ensure work branch exists and check it out
git branch $WORK_BRANCH
git checkout $WORK_BRANCH

# Reset to remote work branch, or the main branch if the work branch doesn't exist
git reset --hard origin/$WORK_BRANCH || git reset --hard origin/$MAIN_BRANCH

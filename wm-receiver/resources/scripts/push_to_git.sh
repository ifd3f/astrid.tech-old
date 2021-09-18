#!/usr/bin/env sh

set -o xtrace

MSG=$1
REMOTE=$2
BRANCH=$3

# Add all files and commit
git add -A
git commit -m "$MSG"

# Push to the specified remote
git remote set-url origin "$REMOTE"
git push -f origin "$BRANCH"

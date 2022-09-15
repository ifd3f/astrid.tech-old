#!/usr/bin/env bash

# https://stackoverflow.com/a/14203146
POSITIONAL=()
NO_COMMIT=0
NO_PUSH=0

while [[ $# -gt 0 ]]; do
  key="$1"

  case $key in
    -h|--help)
      echo "--no-commit to not immediately commit (implies --no-push)"
      echo "--no-push to not immediately push"
      exit 0
      ;;
    -c|--no-commit)
      NO_COMMIT=1
      NO_PUSH=1
      shift # past value
      ;;
    -p|--no-push)
      NO_PUSH=1
      shift # past value
      ;;
    *)    # unknown option
      POSITIONAL+=("$1") # save it in an array for later
      shift # past argument
      ;;
  esac
done

set -- "${POSITIONAL[@]}" # restore positional parameters

slug="$1"
if [ -z $slug ]; then 
    echo "No slug provided. Usage: $0 slug-name-here"
    exit 1
fi

frontendPath="$(dirname "$0")"

contentRoot="$frontendPath/content"
dateField=$(date --rfc-3339=seconds)
daySubFolder=$(date -u +"%Y/%m/%d")
dayFolder="$contentRoot/blog/$daySubFolder"
mkdir -p $dayFolder

lastOrdinal=$(ls $dayFolder | sort -V | tail -n 1)

if [ -z $lastOrdinal ]; then 
    ordinal="0"
else
    ordinal=$(($lastOrdinal + 1))
fi
echo $ordinal

postFolder="$dayFolder/$ordinal"
postPath="$postFolder/$slug.md"

tempfile=$(mktemp)
original=$(mktemp)
cat > $original <<EOF
---
title: null
date: $dateField
ordinal: $ordinal
tags: []
---
EOF
cat $original > $tempfile

echo "Will create post at $postPath"
EDITOR="${EDITOR:-vi}"
$EDITOR $tempfile

if cmp -s "$tempfile" "$original"; then
    echo "No change, cancelling operation"
    exit 1
fi

mkdir -p $postFolder
mv $tempfile $postPath
npx prettier -w $postPath --config content/.prettierrc.yml

if [ $NO_COMMIT -eq 1 ]; then
    echo "--no-commit was given, skipping commit"
    exit 0
fi

git add $postPath
git commit -m "Create post $postPath"
git fetch && git rebase

if [ $NO_PUSH -eq 1 ]; then
    echo "--no-push was given, skipping push"
    exit 0
fi
git push

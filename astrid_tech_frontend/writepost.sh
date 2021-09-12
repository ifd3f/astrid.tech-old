#!/usr/bin/env bash

slug=$1
if [ -z $slug ]; then 
    echo "No slug provided. Usage: $0 slug-name-here"
    exit 1
fi

contentRoot="content"
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
date: $dateField
ordinal: $ordinal
---
EOF
cat $original > $tempfile

EDITOR="${EDITOR:-vi}"
$EDITOR $tempfile

if cmp -s "$tempfile" "$original"; then
    echo "No change, cancelling operation"
    exit 1
fi

mkdir -p $postFolder
mv $tempfile $postPath
npx prettier -w $postPath
git add $postPath
git commit -m "Create post $postPath"
git fetch && git rebase
git push

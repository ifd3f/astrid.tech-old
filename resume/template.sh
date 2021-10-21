#!/bin/bash

WORK=(
    ../content/work/facebook.yaml
    ../content/work/micro-vu.yaml
    ../content/work/fabtime.yaml
)

ctx=$( yq -r '{work: [inputs|.]}' ${WORK[@]} )

mkdir -p gen/
json2latex <(echo "$ctx") resumedata gen/sections.tex
#!/bin/bash

echo "Pulling docker image"
docker pull ifd3f/astrid_tech_frontend

echo "Building"
docker run ifd3f/astrid_tech_frontend --mount "$1:/app/content" --mount "$2:/app/out"
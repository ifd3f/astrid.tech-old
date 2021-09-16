#!/bin/bash

echo "Pulling docker image"
docker pull astridyu/astrid_tech_frontend

echo "Building"
docker run astridyu/astrid_tech_frontend --mount "$1:/app/content" --mount "$2:/app/out"
# This Dockerfile builds the static site from a mounted content volume.

FROM node:14 AS base
WORKDIR /app
COPY ["package.json", "tsconfig.json", ".prettierignore", ".prettierrc", ".eslintrc.json", "yarn.lock", "./"]
RUN yarn install

COPY . .
# Needs content/ and public/ mounted
CMD gatsby build

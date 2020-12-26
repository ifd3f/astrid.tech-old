FROM node:14 AS base
WORKDIR /build
COPY ["package.json", "tsconfig.json", ".prettierignore", ".prettierrc", ".eslintrc.json", "yarn.lock", "./"]
RUN yarn install

FROM base AS development
WORKDIR /build
CMD gatsby develop

FROM base AS production
WORKDIR /build
COPY plugins ./plugins
COPY src ./src
COPY static ./static
COPY assets ./assets
# Needs content/ and public/ mounted
CMD gatsby build

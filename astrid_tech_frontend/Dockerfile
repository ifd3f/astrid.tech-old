FROM node:stretch

WORKDIR /app

RUN apt-get update && apt-get install git -y

COPY package.json .
RUN yarn install

COPY . .
CMD yarn build
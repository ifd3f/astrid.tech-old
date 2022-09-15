#!/usr/bin/env bash

fatal_error() {
    echo $1
    exit 1
}

if [ -z "$GIT_EMAIL" ]; then 
    fatal_error "Error: must provide GIT_EMAIL"
fi
if [ -z "$GIT_NAME" ]; then 
    fatal_error "Error: must provide GIT_NAME"
fi

git config --global user.email "$GIT_EMAIL"
git config --global user.name "$GIT_NAME"

SSH_RSA_KEY="./id_rsa"
if [ -f "$SSH_RSA_KEY" ]; then
    echo "User provided a RSA key, installing it"
    mkdir -p ~/.ssh
    cp "$SSH_RSA_KEY" ~/.ssh/id_rsa

    chmod 700 ~/.ssh
    chmod 600 ~/.ssh/id_rsa
fi

SSH_KNOWN_HOSTS="./known_hosts"
if [ -f "$SSH_KNOWN_HOSTS" ]; then
    echo "User provided a known_hosts, installing it"
    mkdir -p ~/.ssh
    cp "$SSH_KNOWN_HOSTS" ~/.ssh/known_hosts

    chmod 700 ~/.ssh
    chmod 644 ~/.ssh/known_hosts
fi

export DATABASE_URL=./data/data.db
export REPO_DIR=./repo
export ROCKET_ADDRESS="0.0.0.0"

wm-receiver

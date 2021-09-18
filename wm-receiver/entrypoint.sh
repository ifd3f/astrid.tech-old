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

if [ ! -z "$SSH_PRIVATE_KEY_FILE" ]; then
    echo "Installing SSH key"
    mkdir -p ~/.ssh
    cp "$SSH_PRIVATE_KEY_FILE" ~/.ssh/id_rsa

    chmod 700 ~/.ssh
    chmod 600 ~/.ssh/id_rsa
fi

wm-receiver

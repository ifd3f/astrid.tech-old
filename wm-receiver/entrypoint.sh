#!/usr/bin/env sh

fatal_error {
    echo $1
    exit 1
}

if [ -z $GIT_EMAIL ]; then 
    fatal_error "Must provide GIT_EMAIL"
fi
if [ -z $GIT_NAME ]; then 
    fatal_error "Must provide GIT_NAME"
fi

git config --global user.email $GIT_EMAIL
git config --global user.name $GIT_NAME

if [ $SSH_PRIVATE_KEY_FILE ]; then
    echo "Installing SSH key"
    mkdir -p ~/.ssh
    cp "$SSH_PRIVATE_KEY_FILE" ~/id_rsa

    chmod 700 ~/.ssh
    chmod 600 ~/id_rsa
fi

wm-receiver

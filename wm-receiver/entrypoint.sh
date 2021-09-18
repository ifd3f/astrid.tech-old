#!/usr/bin/env sh

if [ $SSH_PRIVATE_KEY_FILE ]; then
    mkdir -p ~/.ssh
    cp "$SSH_PRIVATE_KEY_FILE" ~/id_rsa

    chmod 700 ~/.ssh
    chmod 600 ~/id_rsa
fi

wm-receiver

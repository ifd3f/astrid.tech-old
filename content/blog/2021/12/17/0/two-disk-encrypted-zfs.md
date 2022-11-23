---
title: Installing NixOS with multiple encrypted ZFS pools, decrypted with one password
date: 2021-12-16 18:31:27-08:00
ordinal: 0
tags:
  - zfs
  - nixos
  - linux
  - blogumentation
  - guide
---

This article describes how I got a NixOS install working with multiple encrypted ZFS disks.

<!-- excerpt -->

## Motivation

Skip this part if you don't care about me.

I finally installed NixOS on my (formerly) Arch Linux laptop. Doing so presented a couple of challenges.

![Neofetch for my fresh new install.](https://s3.us-west-000.backblazeb2.com/nyaabucket/ab5779d7b3ac85550ada12a8e93ff8a64a39446c7b1db8d5adc1efd1e21ebae1/banana-neofetch.png)

The biggest one was the fact that it had an interesting combination of drives that I wanted to install ZFS on:

- 1TB nVMe SSD with existing Windows install
- 500MB SATA SSD to contain my /home directory

They run at different speeds, and are of different sizes, so it would not be a good idea to have a single zpool with both disks in it. So, I put them in separate zpools.

```
❯ zpool list
NAME      SIZE  ALLOC   FREE  CKPOINT  EXPANDSZ   FRAG    CAP  DEDUP    HEALTH  ALTROOT
bigdisk   466G   262G   204G        -         -     1%    56%  1.00x    ONLINE  -
homie     464G  33.2G   431G        -         -     0%     7%  1.00x    ONLINE  -
```

However, I wanted to encrypt both disks ~~because I felt like LARPing as a paranoid hacker~~ because security is very important.

## How to do it

We'll assume that you're in a NixOS live USB.

### Planning our layout

Because [it's not a good idea to directly encrypt the pool root](https://www.reddit.com/r/zfs/comments/bnvdco/zol_080_encryption_dont_encrypt_the_pool_root/), this is the planned layout for the ZFS pools:

- `bigdisk` - contains parts of the 1TB SSD
  - `bigdisk/enc` - encrypted subtree
    - `bigdisk/enc/nix` - mounted at `/nix`
    - `bigdisk/enc/root` - mounted at `/`
- `homie` - the 500MB SSD
  - `homie/enc` - encrypted subtree
    - `homie/enc/home` - the mounted at `/home`

### Setting up encryption for `bigdisk`

To create `bigdisk/enc` (do *not* create `homie/enc` this way!), run:
```bash
zfs create -o mountpoint=none -o encryption=aes-256-gcm -o keyformat=passphrase -o keylocation=prompt bigdisk/enc
```

Explanation:

- `-o mountpoint=none` - Prevent it from being mounted.
- `-o encryption=aes-256-gcm` - Enable encryption.
- `-o keyformat=passphrase` - The key to decrypt this pool will come in the form of a passphrase.
- `-o keylocation=prompt` - ZFS will acquire this passphrase by asking the user when you `zfs load-key`.

Enter in your desired passphrase and you've created the encrypted subtree.

### Creating and mounting `/` and `/nix`

Create your datasets and mount them like so:
```bash
zfs create -o mountpoint=legacy bigdisk/enc/root
mount -t zfs bigdisk/enc/root /mnt

zfs create -o mountpoint=legacy bigdisk/enc/nix
mkdir -p /mnt/nix
mount -t zfs bigdisk/enc/nix /mnt/nix
```

Note that `-o mountpoint=legacy` is there to have NixOS manage the mounting, rather than having ZFS mount things for us. Additionally, note that we did not have to specify encryption settings, because the parent pool is already encrypted.

### Setting up encryption for `homie`

We are going to have ZFS decrypt `homie/enc` by reading a passphrase from a file (see footnote[^1] for why this must be done). First, create your passphrase by writing a random string of characters to a file:
```
mkdir /mnt/volkeys
chmod 500 /mnt/volkeys

# This just generates a random string of 32 characters. https://gist.github.com/earthgecko/3089509
cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 32 | head -n 1 > /mnt/volkeys/homie.key
chmod 400 /mnt/volkeys/homie.key
```

[^1]: The first thing I tried was setting `homie/enc` to use `keylocation=prompt`. While this may work for `bigdisk/enc` because that's the root pool, NixOS will only ask you for the password of one pool, and not the others. If you do set `keylocation=prompt` anyways, then SystemD will hang while trying to mount `homie/enc` because ZFS is trying to ask you for a password. Reading the password from a file will be secure because the password is on an encrypted partition already.

You can technically use raw bytes for this, but I opted for alphanumeric ASCII chars so that I can store it in a password manager and potentially recover it if I ever lose that file.

Now, execute
```bash
zfs create -o mountpoint=none -o encryption=aes-256-gcm -o keyformat=passphrase -o keylocation=prompt bigdisk/enc
```

Explanation:

- `-o mountpoint=none` - Prevent it from being mounted.
- `-o encryption=aes-256-gcm` - Enable encryption.
- `-o keyformat=passphrase` - The key to decrypt this pool will come in the form of a passphrase.
- `-o keylocation=file:///mnt/volkeys/homie.key` - ZFS will read the passphrase by from `/mnt/volkeys/homie.key` when you run `zfs load-key`

To double-check that you can correctly load and unload the key:
```
zfs unload-key bigdisk/enc
zfs load-key bigdisk/enc
```

### Create and mount `/home`

Similar to before, do it like so:
```bash
zfs create -o mountpoint=legacy homie/enc/home
mkdir -p /mnt/nix
mount -t zfs homie/enc/home /mnt/nix
```

### Install NixOS

At this point, you can pretty much install NixOS like normal, with configurations following [the ZFS guide on the NixOS wiki](https://nixos.wiki/wiki/ZFS).

### Changing `keylocation` to prevent errors on boot

You might remember earlier that we set that flag `-o keylocation=file:///mnt/volkeys/homie.key`. Unfortunately, if you rebooted with that, ZFS will try to read from `/mnt/volkeys/homie.key` when the password file is actually at `/volkeys/homie.key` by that time!

To fix this, run
```bash
zfs set keylocation=file:///volkeys/homie.key homie/enc
```

### What should happen when you reboot

1. NixOS starts up from the bootloader into Stage 1.
2. To unlock the root partition, NixOS asks you to enter in the password for `bigdisk/enc`.
3. If you successfully enter in the password, ZFS will decrypt `bigdisk/enc`, mount `bigdisk/enc/root` to `/`, and proceed to Stage 2.
4. In Stage 2, SystemD will ask ZFS to unlock `homie/enc`. To do so, it will read the password for `homie/enc` from `/volkeys/homie.key`, and successfully decrypt `homie/enc`.
5. Finally, SystemD will mount `homie/enc/home` to `/home`.
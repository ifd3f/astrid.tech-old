---
title: Miscellaneous Cluster Updates
date: 2021-01-26 22:29:00-08:00
description: Solution LF Problem
tags:
  - /projects/hyposcale-cluster
  - kubernetes
  - docker
  - server
  - devops
  - elk
  - proxmox
---

Look at all of these single-board computers lying around. What are they even doing? _Literally nothing._ Let's put them to good use.

![Two Pi 3's, one Pi 2, and one Orange Pi one](./raw-pis.jpeg)

## Raspberry Pi SD Cards

I got the latest [Raspbian Buster Lite image](https://www.raspberrypi.org/software/operating-systems/) and extracted the image. To make things easier on myself, I decided to enable SSH-on-first-boot for the image so that I can just brainlessly `dd` everything onto SD cards.

First, I executed

```zsh
losetup -P /dev/loop99 2021-01-11-raspios-buster-armhf-lite.img
```

to mount the image on a loop device. 99 was chosen because snapd makes a goddamn million loop devices.

`lsblk` returned the following:

```
NAME                  MAJ:MIN RM   SIZE RO TYPE MOUNTPOINT
loop0                   7:0    0 818.2M  1 loop /snap/android-studio/98
loop1                   7:1    0 373.8M  1 loop /snap/anbox/186
...
loop47                  7:47   0 135.7M  1 loop /snap/chromium/1466
loop99                  7:99   0   1.8G  0 loop
├─loop99p1            259:7    0   256M  0 part
└─loop99p2            259:8    0   1.5G  0 part
...
```

Perfect! Once that was done, it was a simple

```zsh
mkdir -p /tmp/bootsd
mount /dev/loop99p1 /tmp/bootsd
touch /tmp/bootsd/ssh
```

and the SD card image was ready. To clean everything up:

```zsh
umount /tmp/bootsd
losetup -d /dev/loop99
```

Finally, I plugged in the SD cards one by one and flashed them with

```zsh
dd bs=64M of=/dev/sdb if=2021-01-11-raspios-buster-armhf-lite.img
```

As soon as I booted each of them, and they were assigned DHCP addresses, I SSH'ed into them to change the password because security.

## Orange Pi SD Card

The Orange Pi One was set up in a similar fashion, except that I got the Armbian image from [the Orange Pi website](http://www.orangepi.org/downloadresources/) (scroll like, really far for the OPi One).

## Getting k3s on them

I used `ssh-copy-id` to install my public key on all of the devices for passwordless login, and then used [k3sup](https://github.com/alexellis/k3sup) for quickly installing k3s on them:

```
k3sup join --sudo --server-user rancher --user pi --server-ip <my server's address> --ip <the pi's address>
```

Forking multiple instances of k3sup made life easier.

So now, I have **7 whole nodes on my kubernetes cluster!** Fear my power!

```
$ kubectl get node
NAME         STATUS     ROLES    AGE     VERSION
zerg-1       NotReady   <none>   2d1h    v1.19.7+k3s1  # rpi
k3os-28502   Ready      master   6d2h    v1.19.5+k3s2
orangepi     Ready      <none>   4d10h   v1.19.7+k3s1  # opi
zerg2        Ready      <none>   2d1h    v1.19.7+k3s1  # rpi
zerg3        Ready      <none>   2d1h    v1.19.7+k3s1  # rpi
k3os-10009   Ready      <none>   5d23h   v1.19.5+k3s2
k3os-3502    Ready      <none>   3h13m   v1.19.5+k3s2
```

Please ignore the inconsistent dashing scheme on the zerg\* series.

![Look at these SBC's!](./nodezzz.jpeg)

Now what are they running? ...unfortunately, still, absolutely nothing. Once again, my k3s cluster is a solution looking for a problem.

In my next post, I'll talk about my Grafana + Prometheus setup.

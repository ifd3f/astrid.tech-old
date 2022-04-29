---
title: A horrible workaround for my bootloader deleting itself from EFI
description: Eye bleach suggested afterwards
date: 2021-10-18 19:20:01-07:00
ordinal: 0
tags:
  - jank
  - incompetency
  - linux
  - blogumentation
---

What is this systemd service I have on my computer?

```
❯ cat /etc/systemd/system/install-bootloader-on-shutdown.service
```

<!-- excerpt -->

```conf
[Unit]
Description=Install bootloader on shutdown
DefaultDependencies=no
Before=shutdown.target reboot.target halt.target

[Service]
Type=oneshot
ExecStart=bootctl install

[Install]
WantedBy=multi-user.target
```

In short, it's a horrible workaround for an absolutely stupid problem on my
computer.

## The problem

In the beginning, I dual-booted Ubuntu and Windows. But one day, I got bored of
Ubuntu and installed Arch (btw) sometime around 2021-03-20, when spring break
started. I wanted to preserve my Windows system and /home folder, so I tried to
work around my existing setup.

I got Arch installed perfectly fine! However, what ended up happening sometimes
was systemd-boot getting completely wiped from my EFI entries every time I
restarted my computer, meaning I could only boot into Windows unless I pulled
out my Arch USB and ran

```bash
mount /dev/mapper/bigdiskenergy-root /mnt
mount /dev/nvme0n1p1 /mnt/boot
arch-chroot /mnt
bootctl install
```

## Possible solutions

I looked around a fix. First, I verified that my systemd-boot entries were
correct:

```
❯ cat /boot/loader/loader.conf
```

```conf
timeout 3
console-mode keep
default arch.conf
editor 0
```

```
❯ cat /boot/loader/entries/arch.conf
```

```conf
title	Arch Linux
linux 	/vmlinuz-linux
initrd	/intel-ucode.img
initrd	/initramfs-linux.img
options root=/dev/mapper/bigdiskenergy-root rw intel_iommu=on
```

They seemed correct, and I had even copied the entries of others using a root on
LVM. So that probably wasn't it.

Next, I tried installing GRUB instead of systemd-boot. However, GRUB's boot
entry also kept getting deleted, so it wasn't a problem with systemd-boot.

So I went back to systemd-boot because it was lighter and I liked it better. As
a workaround, I ran `bootctl install` every time I shut down my computer, but
whenever I forgot to do that, I had to pull out my live USB again, and run the
good ol'

```bash
mount /dev/mapper/bigdiskenergy-root /mnt
mount /dev/nvme0n1p1 /mnt/boot
arch-chroot /mnt
bootctl install
```

Clearly, this was not sustainable.

## What I settled on

This brings us back to `install-bootloader-on-shutdown.service`:

```conf
[Unit]
Description=Install bootloader on shutdown
DefaultDependencies=no
Before=shutdown.target reboot.target halt.target

[Service]
Type=oneshot
ExecStart=bootctl install

[Install]
WantedBy=multi-user.target
```

It does what it says on the tin: install the bootloader on shutdown. This seems
to fix my problem in the worst way possible, and I'm never touching this problem
ever again.

---
title: How my NixOS-y infrastructure is going so far and my future plans
description: aka infra review, twenty-twenty-two
date: 2022-03-24 15:25:03-07:00
ordinal: 1
tags: 
  - /projects/infrastructure
  - nixos
  - iac
  - lxd
---

Here's a summary of my impressions of NixOS after about 6 months of using it on my daily driver machines and servers.

In short, all of my machines run NixOS and I have a central infrastructure configuration.

## Pros of NixOS

### Extremely reproducible, reusable, and portable configurations

This is, *by far*, the biggest pro and the reason why I like NixOS so much. It takes the idea of infrastructure as code (IaC) to the extreme; your OS's entire configuration, including config files, systemd services, packages, boot files, etc. are written in a Turing-complete language. I never have to worry about stray configurations screwing up my system, and every time I make a change to my system it gets recorded in Git. I can even reuse those changes in all my other machines.

### Extremely flexible configuration

The configuration language is Turing-complete.

### `home-manager` is awesome and portable

## Cons of NixOS

### FHS-dependent software needs a crapton of workarounds

If it's not packaged for NixOS, it probably doesn't work out-of-the-box. For example, AppImages don't work. Generic Linux binaries don't even work because they probably depend on some dynamic library somewhere in your system that NixOS does not provide, unless you build an environment specifically containing that dynamic library. 

Due to these environment issues, you might need try some workarounds. For example, I was trying to run a Rust-based password cracker I wrote for my cybersecurity class. When it was built normally, it didn't work. I had to build it statically-linked (using musl) in order for it to actually work.

For example, to run Anaconda, I have a Centos 8 LXC on my server that I SSH into. Much of my development work now happens in VSCode devcontainers. Perhaps there's better ways to go about it, but that's what I'm doing for now.

### Packaging often breaks, more often than Arch

### Documentation is a little lacking

## Next steps

NixOS is great, and I will continue using it. However, there are some things that unfortunately don't work very well.

I'll likely look into FHS stuff to see if I can improve compatibility.



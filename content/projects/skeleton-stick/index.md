---
title: Skeleton Stick
status: complete
description: A hardware password manager
startDate: 2021-12-15
endDate: 2022-02-15
thumbnail: ./unlocked.jpg
source:
  - https://github.com/astralbijection/skeleton_stick
tags:
  - cybersecurity
  - raspberry-pi
  - python
  - rust
  - latex
---

Skeleton Stick is a hardware password manager designed to bridge the gap between software password managers (such as LastPass and Bitwarden) and physical devices where those aren't available (like public or corporate computers).

It's based on a Raspberry Pi Zero. Although it's only a proof of concept implemented in Python, it's been quite a cool concept, so I might make a v2 of it at some point.

## Whitepaper

[Click here to read the "whitepaper" I wrote for class.](./report.pdf) It's written in $\LaTeX$ and serves as a fairly comprehensive summary of what I've implemented.

## Future plans

Currently, it takes way too long too boot after being plugged in (over 60 seconds) because it's written in Python and the main process is spawned as a subprocess of SystemD. I'll likely rewrite it in Rust and implement my own init process to lower the boot times.

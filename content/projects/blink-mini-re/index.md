---
title: Blink Mini Reverse Engineering
status: wip
featured: true
description: Hacking apart an Amazon camera to make it run my own code
startDate: 2022-07-07
endDate: null
tags:
  - reverse-engineering
  - electrical-engineering
  - cybersecurity
  - ghidra
  - jupyter
  - python
  - statistics
  - soldering
---

![Blink Mini camera from Amazon.](./blink-mini.jpg)

![A camera that has been violently opened up.](https://astrid.tech/_/2022/07/07/0/opening-attempt-2.jpg)

I attended the [2022 Cybertruck Challenge](https://www.cybertruckchallenge.org/)
in early July, in which I learned about hacking, then proceeded to hack trucks
and truck accessories.

With what I gleaned from the 20 hours of instruction and 20 hours of hacking and
reverse engineering, I decided that I should apply what I learned to a real
device! These \$20 Blink Mini cameras seemed like perfect victims... at the
time.

Big thanks to [Ada](https://twitter.com/lacecard) and
[Erin](https://twitter.com/e_er1n) for all their help with this project!

## Blog posts about my journey so far

1. [Disassembling an Amazon Blink Mini camera](https://astrid.tech/2022/07/07/0/blink-mini-disassembly/)
2. [Blink Mini RE, Part 2 -- Desoldering and dumping the ROM](https://astrid.tech/2022/07/13/0/blink-mini-dumping/)
3. [Blink Mini RE, Part 3 -- Staring into the eye of the binary](https://astrid.tech/2022/08/03/0/blink-mini-fw-analysis/)

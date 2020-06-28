---
type: blog

title: Parallels Between Digital Logic and React
date: "2020-06-16T22:19:00-0700"
description: State and pure functions that change state
tags:
  - notes
  - computer-science
  - computer-engineering
  - fpgas
  - hdl
  - verilog
  - systemverilog
  - react-js
---

Digital logic on a FPGA is about as low-level as it gets. You're literally manipulating individual bits and trying to get them to behave. Signal propagation times are a fairly big worry, clock domains need to be crossed, and BRAM needs to be conserved.

Web development, especially when there's React there to abstract much of your code, is about as high-level as it gets. You barely even have to care about RAM, not to mention disk space.

![haha node_modules big](./node_modules_meme.png)

In React, you basically have components that

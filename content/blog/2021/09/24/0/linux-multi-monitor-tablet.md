---
title: Locking Drawing Tablet on Single Monitor
date: 2021-09-23 21:07:16-07:00
ordinal: 0
tags:
  - blogumentation
  - drawing-tablet
  - linux
---

To lock a drawing tablet to a single monitor, the command is

```
xinput map-to-output $STYLUS $MONITOR
```

where `$STYLUS` is gottten from `xinput`:

```
❯ xinput
⎡ Virtual core pointer                    	id=2	[master pointer  (3)]
⎜   ↳ ...
⎜   ↳ HID 256c:006d Pen Pen (0)               	id=26	[slave  pointer  (2)]
⎜   ↳ ...
⎣ Virtual core keyboard                   	id=3	[master keyboard (2)]
    ↳ ...
```

and `$MONITOR` is gotten from `xrandr`:

```
❯ xrandr --listactivemonitors
Monitors: 3
 0: +*DP-1-2 3840/621x2160/341+0+0  DP-1-2
 1: +eDP1 1920/340x1080/190+3840+1440  eDP1
 2: +HDMI-1-0 2560/530x1440/290+3840+0  HDMI-1-0
```

Reference: [StackOverflow](https://askubuntu.com/a/855608)

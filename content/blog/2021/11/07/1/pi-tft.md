---
title: Testing a Raspberry Pi 4 with SPI TFT Screen
date: 2021-11-07 01:34:30-07:00
ordinal: 1
tags:
  - raspberry-pi
  - linux
  - lab-notes
---

Today, my Raspberry Pi 4 and
[TFT/case bundle](https://www.ebay.com/itm/123778645736) came in the mail, so I
decided to give it a spin and see how it works. This will be used for an
upcoming project that I will not spoil.

## Assembly

The case came with 4 screws to screw the Pi into the bottom of the case.

![The Pi, seated in the bottom.](https://s3.us-west-000.backblazeb2.com/nyaabucket/19847881bfeecdcc72dd0728267b830ca0aae293795cabff3b8f996dcc8d7f20/assembly-1.jpg)

Then, I slotted the TFT screen onto the GPIO pins.

![The Pi, with a TFT screen on top](https://s3.us-west-000.backblazeb2.com/nyaabucket/1ed8603ae36c8c9252b0c7446e04a5340c1cc89bf58db14c4d97296f2107ee73/assembly-2.jpg)

Finally, the upper part of the case had a cover (which I assume is for when you
don't have a TFT screen). I removed that cover, and the case simply snapped into
place.

![The Pi, with a TFT screen on top, enclosed in the case](https://s3.us-west-000.backblazeb2.com/nyaabucket/06e1abf4b5dc9ba2fa820a0f7611ba272069aafcbf82fecb6091f916b0ef930f/assembly-3.jpg)

## Testing the TFT screen

I found a spare micro SD card to install
[Raspberry Pi OS](https://www.raspberrypi.com/software/operating-systems/) onto.

For my first attempt, I tried Raspberry Pi OS Lite (no X11, no desktop). Then,
following some (modified) instructions from the TFT store page, I ran the
following commands on the Pi:

```bash
wget https://github.com/goodtft/LCD-show/archive/refs/heads/master.zip
unzip master
cd LCD-show/
sudo ./LCD35-show
```

I expected it to render the standard systemd services output, but the script
failed because I guess it relied on X11 being there. Raspberry Pi OS with
Desktop worked, however, and I was able to see the desktop rendered on the TFT
screen and interact with it via the resistive touch sensor.

![The desktop showing on the Pi, with stylus interaction!)(./working-tft.jpg)

## Power consumption

I then proceeded to measure the power consumption of the Pi.

Desktop just loaded: 4.82V, 0.76A, 13mAh
![](https://s3.us-west-000.backblazeb2.com/nyaabucket/3a19b3b2bbd484f96d90cd0c6f149a003d030506d1eb3774d82c2eaeefb6fc82/desktop-just-loaded.jpg)

Desktop finished loading: 4.90V, 0.58A, 15mAh
![](https://s3.us-west-000.backblazeb2.com/nyaabucket/bc8ef1aa50bb65355d35f3f516d5fdad8b4620ea546c3fbc638217c498842ee7/desktop-finished-loading.jpg)

I noticed that my tester is actually USB 2.0, which only provides a maximum of
10W, which may mean these results are inaccurate. However, maybe my computer's
power output isn't that high either.

## Next steps

- I want to see if I can get the kernel module and stuff working on NixOS. That
  will make configuration management less of a headache.
- I want to underclock the Pi to reduce power consumption.

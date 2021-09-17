---
type: project

title: QuadFrost 3D Printer System
status: complete
description: Control system for my 3D printer enclosure's lights, filter, and LCD
featured: true
startDate: 2019-08-14
endDate: 2019-08-18
tags:
  - node-js
  - websockets
  - javascript
  - html
  - css
  - socket-io
  - electrical-engineering
  - mechanical-engineering
  - fusion-360
  - circuit-design
url: null
source:
  - https://github.com/astralbijection/QuadFrost
thumbnail: ./3dp-colors.webp
---

It's yet another IKEA 3D printer enclosure, but with custom electronics on it!
There's an [OctoPrint](https://octoprint.org/) plugin I wrote, that communicates
with an Arduino I programmed, which sends and receives signals of all sorts, so
the whole setup is less boring!

![The full setup!](./full-enclosure.jpg)

I also have a script that automatically runs every few minutes and takes a
picture of the printer. This image updates regularly.

![Image of my 3D printer right now](https://api.astrid.tech/3dprinter/1/snapshot.jpg)

## Feature List

- LED strips!
- Motorized "air filter"
- Limit switch to detect the front panel
- Thermometer to measure air temperature
- LCD display that shows:
  - Printer status
  - Print progress as a percentage
  - Air temperature

![Close-up of the LCD.](./lcd.jpg)

![The internal parts.](./internals.jpg)

## Why?

The Monoprice Maker Select i3 v1 has no enclosure. So, this is how I used to
print ABS.

![OSHA-certified to not catch on fire!](./old.jpg)

This is obviously a non-ideal setup.

My mom also wanted to get rid of a pair of LACK corner tables, so I decided to
hop on the
[LACK Enclosure train](https://blog.prusaprinters.org/cheap-simple-3d-printer-enclosure_7785/)
and build my own!

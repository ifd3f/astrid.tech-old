---
type: project

title: Goggle Epoxy
status: scrapped
description: Google Glass, but it's cheaper, engineered worse, and non-functional
startDate: 2018-06-01
endDate: 2018-08-15
tags:
  - sensor-fusion
  - motion-sensing
  - java
  - kotlin
  - raspberry-pi
  - augmented-reality
  - mechanical-engineering
  - 3d-printing
  - resin-casting
  - internet-of-things
  - wearable
  - optics
  - ssd1306
url: null
source: [https://github.com/astralbijection/Goggle-Epoxy]
thumbnail: ./circuitry.jpg
---

![The circuitry.](./circuitry.jpg)

Low-cost experimental AR headset similar to the Google Glass.

At its heart was a Raspberry Pi Zero that drew images on an OLED display. The display projected images on the casted epoxy prism, which reflected images onto the user's eye. Its frame would have been made from 3D printed parts.

![The resin mold.](./prism-casting.jpg)

Unfortunately, the project was unsuccessful due to limitations in the epoxy casting process: the surface of the prism was too rough and opaque for any meaningful reflections.

If I were to remake the project, I would not have attempted to make it in the same way that Google did; I would use a method of reflecting OLED images into the user's eye other than the prism-based method they used.

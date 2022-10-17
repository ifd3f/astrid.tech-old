---
type: project

title: KiwiBot
status: scrapped
description: An attempt at a fully 3D-printed kiwi drive robot
startDate: 2017-10-06
endDate: 2017-10-24
tags:
  - kotlin
  - java
  - cpp
  - android
  - bluetooth
  - hc-06
  - arduino
  - control-systems
  - cr-servo
  - fusion-360
  - robotics
  - mechanical-engineering
url: null
source: [https://github.com/ifd3f/KiwiBot]
thumbnail: ./kiwibot-thumb.jpg
---

<iframe src="https://myhub.autodesk360.com/ue28d9dcb/shares/public/SH56a43QTfd62c1cd968eb1efe20e185a001?mode=embed" width="640" height="480" allowfullscreen="true" webkitallowfullscreen="true" mozallowfullscreen="true"  frameborder="0"></iframe>

<iframe src="https://myhub.autodesk360.com/ue28d9dcb/shares/public/SH56a43QTfd62c1cd968deeb555105c15399?mode=embed" width="640" height="480" allowfullscreen="true" webkitallowfullscreen="true" mozallowfullscreen="true"  frameborder="0"></iframe>

![The robot, fully assembled](./kiwibot-raw.jpg)

I started this project because I had figured out a way to 3D print omniwheels
and I wanted to make an omnidirectional robot with them. Unfortunately, this
project was scrapped because it turns out that CR servos do not offer very
precise movement required for an omniwheel drive.

However, I was able to program a PID system to make the robot keep facing the
same direction even when it is disturbed, as seen in the following GIF.

<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/Js2rsqtkQ4EMXftQDa" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/Js2rsqtkQ4EMXftQDa">via GIPHY</a></p>

## Prototypes

Here is an early prototype wheel. It was found to be too slippery and abandoned.

![An early prototype wheel.](./wheel-proto1.jpg)

The wheels I used had TPU for the rollers so that it could have somewhat more
traction. In addition, the TPU was hollow to provide a sort of bounciness to it.

![A view of the lower deck, showing the orange TPU mini wheels.](./lower-deck-assembled.jpg)

## Assembly

The lower deck holds the motors.

![An image of the lower deck](./lower-deck.jpg)

The upper deck holds the controls. It is attached to the lower deck via a M4
bolt and nut that is placed in a slot.

![The bolt and nut](./bolt-assembly-detail.jpg)

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
url: null
source: [https://github.com/Plenglin/KiwiBot]
thumbnail: ./kiwibot-thumb.jpg
---

![The robot, fully assembled](./kiwibot-raw.jpg)

I started this project because I had figured out a way to 3D print omniwheels and I wanted to make an omnidirectional robot with them. Unfortunately, this project was scrapped because it turns out that CR servos do not offer very precise movement required for an omniwheel drive. However, I was able to write a PID system to make the robot keep facing the same direction even when it is disturbed, as seen in the following GIF.

<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/Js2rsqtkQ4EMXftQDa" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/Js2rsqtkQ4EMXftQDa">via GIPHY</a></p>

## Prototypes

Here is an early prototype wheel. It was found to be too slippery and abandoned.

![An early prototype wheel.](./wheel-proto1.jpg)

The wheels I used had TPU for the rollers so that it could have somewhat more traction. In addition, the TPU was hollow to provide a sort of bounciness to it.

![A view of the lower deck, showing the orange TPU mini wheels.](./lower-deck-assembled.jpg)

## Assembly

The lower, motor deck.

![An image of the lower deck](./lower-deck.jpg)

The upper, control deck is attached to the lower deck via a M4 bolt and nut that is placed in a slot.

![The bolt and nut](./bolt-assembly-detail.jpg)

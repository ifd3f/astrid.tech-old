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
thumbnail: https://s3.us-west-000.backblazeb2.com/nyaabucket/fd70909b65df3a9461d16a009c790bacf13c826f07eea0e7c688e6f23005e292/kiwibot-thumb.jpg
---

<iframe src="https://myhub.autodesk360.com/ue28d9dcb/shares/public/SH56a43QTfd62c1cd968eb1efe20e185a001?mode=embed" width="640" height="480" allowfullscreen="true" webkitallowfullscreen="true" mozallowfullscreen="true"  frameborder="0"></iframe>

<iframe src="https://myhub.autodesk360.com/ue28d9dcb/shares/public/SH56a43QTfd62c1cd968deeb555105c15399?mode=embed" width="640" height="480" allowfullscreen="true" webkitallowfullscreen="true" mozallowfullscreen="true"  frameborder="0"></iframe>

![The robot, fully assembled](https://s3.us-west-000.backblazeb2.com/nyaabucket/e0f148da21cae912b71935617624b4cc83de15f7236dd33f7a0a2c66eca80406/kiwibot-raw.jpg)

I started this project because I had figured out a way to 3D print omniwheels
and I wanted to make an omnidirectional robot with them. Unfortunately, this
project was scrapped because it turns out that CR servos do not offer very
precise movement required for an omniwheel drive.

However, I was able to program a PID system to make the robot keep facing the
same direction even when it is disturbed, as seen in the following GIF.

<div style="width:100%;height:0;padding-bottom:56%;position:relative;"><iframe src="https://giphy.com/embed/Js2rsqtkQ4EMXftQDa" width="100%" height="100%" style="position:absolute" frameBorder="0" class="giphy-embed" allowFullScreen></iframe></div><p><a href="https://giphy.com/gifs/Js2rsqtkQ4EMXftQDa">via GIPHY</a></p>

## Prototypes

Here is an early prototype wheel. It was found to be too slippery and abandoned.

![An early prototype wheel.](https://s3.us-west-000.backblazeb2.com/nyaabucket/69dc6695244a6b6b087cfd2f292d24583956f386707eac819c3654f572cc6a5e/wheel-proto1.jpg)

The wheels I used had TPU for the rollers so that it could have somewhat more
traction. In addition, the TPU was hollow to provide a sort of bounciness to it.

![A view of the lower deck, showing the orange TPU mini wheels.](https://s3.us-west-000.backblazeb2.com/nyaabucket/c75a8b9964ac123aa181fb0f4da45b9b46cdcf8261412dd924947def2a049b91/lower-deck-assembled.jpg)

## Assembly

The lower deck holds the motors.

![An image of the lower deck](https://s3.us-west-000.backblazeb2.com/nyaabucket/d753cef25321fbeddf1b942276a14252be979836f4b43a241888605f1e650c91/lower-deck.jpg)

The upper deck holds the controls. It is attached to the lower deck via a M4
bolt and nut that is placed in a slot.

![The bolt and nut](https://s3.us-west-000.backblazeb2.com/nyaabucket/2e7fd748b0c1355f7e6d0770436ec90cc7c95015c0dfbf15eeb3f2c0731f03ff/bolt-assembly-detail.jpg)

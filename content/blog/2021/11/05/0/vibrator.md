---
title: Massage wand modification
description: "Spoiler: it ain't used for massage"
ordinal: 0
date: 2021-11-04 23:37:33-07:00
tags:
  - /projects/owostick
  - electrical-engineering
  - arduino
  - teardown
  - nsfw
  - teledildonics
---

What do you do with a waterlogged massage wand? Mod it into an
Arduino-controlled bluetooth vibrator with optional Websocket command relay, of
course!

<!-- excerpt -->

I completed this project back in 2020, but never got around to writing about it.
So here it is!

This article will cover my process of disassembling the original device and
modding it with my custom electronics. For more details about the parts of this
project that are not enclosed inside the wand, see
[the associated project page](/projects/owostick).

## The original device

[This is the original wand I had.](https://www.amazon.com/gp/product/B07T3JSKDP)
For \$20, it's a pretty decent piece of hardware: multiple modes, decent power,
very inexpensive.

![The vibrator, as seen on Amazon.](https://s3.us-west-000.backblazeb2.com/nyaabucket/f389befd2fa4a7551e11924154320c90f3e499258937dc676e059c58ec6e83c2/original.jpg)

However, it became waterlogged one day back in the summer of 2020 due to being
accidentally included in the washing machine.

Once I got home from the apartment I had during my summer internship, around
August 2020 I opened up the device to let it dry.

![The entire device, disassembled.](https://s3.us-west-000.backblazeb2.com/nyaabucket/b3dd22fcad5599981b650945e239201231c7687a32303f756ac80b5d5b4e1ab5/disassembly1.jpg)

![Another image of the disassembled device.](https://s3.us-west-000.backblazeb2.com/nyaabucket/40bb007d18e86040c1a79abb9b02e07f5852d6f81b6c1e93062ab9f15bbf45ea/disassembly2.jpg)

The internals are frankly quite boring but that gave me an idea: what if I just
swapped out the circuit board with my own?

## Ambitious First Idea

From October to November 2020 I tried my hand at designing a new circuit for
this wand. I had a (way-too-ambitious) set of requirements for a new circuit for
this vibrator:

- Can act as a drop-in replacement for the old circuit without hardware
  modification
- Can communicate over Bluetooth and Wifi (via ESP32)
- The existing button slots can be used to interface with it
- Uses the original Li-ion battery for charge and discharge
- Can be charged via the original plug
- Has a capacitive sensor on the head for detecting the user

Here is the KiCAD schematic I designed that meets all of these specifications!

![The schematic.](./uwu-notices-your-hardware-schematic.svg)

### PCB Measurement

I additionally desoldered the central circuit to measure dimensions of the holes
and buttons so I could lay out the components on a PCB.

![The original PCB under a magnifying glass.](https://s3.us-west-000.backblazeb2.com/nyaabucket/c506398eabb6b3981f23e7ecab19ae352a6f4e8679fc8ccffb9cea4e93d4cd1f/orig-pcb-0.jpg)

![Another image of the PCB under a magnifying glass, without the stain in the way.](https://s3.us-west-000.backblazeb2.com/nyaabucket/33c8b085cb15b038ed7414b75b5ee87f334ce7f464dba445bab7f8f2bc1d044f/orig-pcb-no-blur.jpg)

![The rear of the PCB, with a dab of hot glue on it that seemed to come with it.](https://s3.us-west-000.backblazeb2.com/nyaabucket/045c98e20db4e47666db2b62895af2285387b571ad423090c59314a5ed9fc789/orig-pcb-rear.jpg)

Here are the 3D renderings of the replacement PCB I created from the schematic
and reference drawing.

![PCB bottom, oblique view.](https://s3.us-west-000.backblazeb2.com/nyaabucket/6984658be12068e7692fc947147ea4f8a6295fcdff1dd289259fb8390ec20206/uwupcb-bottom-oblique.png)

![PCB top, oblique view.](https://s3.us-west-000.backblazeb2.com/nyaabucket/47d9c667c070a6937a4f0b2871a70e2cf39ee7733d3427bbaa71edd3ed5f5153/uwupcb-top-oblique.png)

![PCB top](https://s3.us-west-000.backblazeb2.com/nyaabucket/8bbc12df960de3469405f66191d3ecc7dfb257c0a8b0d1a3c46761d2cba838d0/uwupcb-top.png)

![PCB bottom](https://s3.us-west-000.backblazeb2.com/nyaabucket/834322973aaa61203dc5cd8f36fb8c501abb812a113b934c3978fcfe8de4e075/uwupcb-bottom.png)

![PCB side](https://s3.us-west-000.backblazeb2.com/nyaabucket/840b56a13518117970959f0edd77e92599a0d8e3fe70cbbe9091aeed93abec2c/uwupcb-side.png)

Yes, that ESP32 is slanted. Otherwise, it would end up overlapping with the
drill holes!

### Postmortem

This design is obviously extremely ambitious, and it's a fairly complex circuit.
I constantly agonized over whether or not all the connections were accounted
for, and whether or not all the capacitors and resistors were correct, so I
ended up never ordering the PCB or carrying out this plan.

## ~~Janky~~ _Pragmatic_ Second Idea

In December, I was fed up with not having a functional massage wand for so long,
so I just decided to make something simple. This time, I greatly cut down on my
requirements:

- Can communicate over Bluetooth (via HC-05)
- Can accept power from Li-Ion battery via external XT60 connector
- Can control motor

Essentially, this was a completely headless device that would only work if it
was connected to Bluetooth. But that's fine, because so long as I can connect to
it, it works.

### Sizing the board

I had an Arduino Pro Mini clone lying around that had been used for a previous
project, but was no longer used. It was soldered directly to a perfboard (no
headers for replaceability, no nothing) so I couldn't find a good repurposing
for it for a while. Thankfully, this project came along and gave it a use!

![Me sizing the board in the wand.](https://s3.us-west-000.backblazeb2.com/nyaabucket/e2919d22ef4d2dbafeb2233fbd4f6cda6e2ed8dac5f33a4d8817ef0390ac78b5/sizing.jpg)

I cut the board in half lengthwise around the Arduino, disconnecting and
desoldering the old wires.

![A different PCB in here, also for measuring the size.](https://s3.us-west-000.backblazeb2.com/nyaabucket/7e9ee2eab517c377381028bd70051ccc8d382d8ddd9f8e501be690f2e1b2ce86/longfit1.jpg)

![The repurposed and reshaped board, fitted into the wand's chassis.](https://s3.us-west-000.backblazeb2.com/nyaabucket/58f164918580eac838ccea0802f8c23719f1656df3634b4c028ebd6c732fa11c/longfit2.jpg)

### Soldering the board

I had an HC-05 from a different project as well, that was no longer in use. So,
I desoldered it off of the headers binding it to the old project.

![The desoldered HC-05.](https://s3.us-west-000.backblazeb2.com/nyaabucket/abac1c39710716aefc5168abf08df6cab27171c30b6294f08444dc0b889d0f25/desolder-hc05.jpg)

However, I _did_ need to attach it to my new board, and unfortnately it's a
rather long module so I had to make some... _slight_ aesthetic compromises to
the board.

![The top of the final soldered perfboard. Note the HC-05 being slanted, and partially overlapping the Arduino.](https://s3.us-west-000.backblazeb2.com/nyaabucket/1708dc2cbc3d5d3266b813b02fa22adf025dd53b8d400e9c88aa522050dfc5d5/owopcb-top.jpg)

Additionally, because the MOSFET couldn't fit in a conventional location, I had
to make some "compromises" there as well.

![The DIP MOSFET, "surface-soldered" onto the rear of the perfboard.](https://s3.us-west-000.backblazeb2.com/nyaabucket/bfbfa44f03f3d2a987df9b721c91b7cd46ca3b13bb505664c148139fbe9a212c/owopcb-bottom.jpg)

Furthermore, there was not enough vertical clearance for the board if you tried
to encase this circuit inside the chassis.

So, I had to destructively modify the case with a wire cutter.

![The modified wand case.](https://s3.us-west-000.backblazeb2.com/nyaabucket/2e60f1b5d48d7037234bff8186932cabf736787a6d5b07d68f271375cbfa8b98/case-mod.jpg)

Once that was all done, all I had to do was solder the PCB to the motor, and I
was done!

![The circuit, oriented in the correct way for soldering!](https://s3.us-west-000.backblazeb2.com/nyaabucket/16ce0c85263a284f93ad75ab4dc7490d50d4b29b00fd9fc262fbeaaed5d3124b/owopcb-final-connection.jpg)

![The reassembled device with external battery!](https://s3.us-west-000.backblazeb2.com/nyaabucket/d9de2ebcbb1f09832f3e9215b75092c967516dd3f7e3fef88c4eb03bb4e46bf7/reassembled.jpg)

### Results

This device is almost a direct upgrade from the previous one.

- You can control it with a phone via Bluetooth connection! With other services
  involved, someone else can even control it (or dare I say, _command_ it) from
  anywhere in the world via a Websocket relay!
  [See the project page for more details about that.](/projects/owostick) The
  uses of this relay system are as of yet unclear.
- Double the voltage means double the motor power!

There are a few drawbacks, however.

- The silicone exterior could not be directly slipped back on. I had to cut an
  opening through it, so it no longer protects the electronics from the external
  environment. To operate it in an environment with fluids, it must be wrapped
  in plastic wrap.
- The external battery makes it bulkier, but that's the price you pay for double
  the power.

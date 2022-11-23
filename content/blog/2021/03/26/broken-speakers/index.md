---
title: My speaker is broken, what do?
date: 2021-03-26 12:33:50-08:00
description:
  Cracking open a hard one with no one but myself because there's a pandemic
  outside
tags:
  - electrical-engineering
---

I have these Z506 speakers that I got for cheap on eBay a couple years ago. They
worked pretty well until one day, the audio was extremely loud and the
potentiometer was not able to turn the sound down, so I had to either run my
audio at 4% or not at all.

![The Z506 Speakers from Logitech.](https://s3.us-west-000.backblazeb2.com/nyaabucket/7d3c977f17798c4f5b5c2409ba789f753d78b3bf51b003b2b75bbbcfae5fda8f/z506.jpg)

That's not good. Let's fix that!

## Wild Conjecturing as to What Went Wrong

I'm not an audio or electrical engineer, but I had a few guesses as to what
might have gone wrong.

- Since the control speaker is connected to the subwoofer with a D-sub
  connector, I suspected that maybe the subwoofer is the one doing the volume
  control, and maybe something on there broke.
- A component in the control speaker may broken.

I decided to open up the control speaker and check that first.

## Opening it up

I removed the six bolts on the front, and slowly pried the case open.

![Opening the case.](https://s3.us-west-000.backblazeb2.com/nyaabucket/9a2bb45719743ba018ad213301b4ba309fec897f75b158bbc9e423e689829b8a/010-open-back.jpg)

I pulled it fully open and disconnected the speaker and the connector. However,
the case was still refusing to budge.

![All the bolts have been removed.](https://s3.us-west-000.backblazeb2.com/nyaabucket/008f7bf2c38ff75dbd434da1e4b3fbc7edc444ba125b94c99e456d780a9cca6a/020-removed-bolts.jpg)

It turns out that I had forgot to free the potentiometer's nut and washer on the
front of the case!

![](https://s3.us-west-000.backblazeb2.com/nyaabucket/bd34e6b1cacc1df91e92a42041f4e25eae1021f6149ce4bb5fd5187335c92867/030-pot-stuck.jpg)

Getting rid of that made the chip much easier to remove.

![](https://s3.us-west-000.backblazeb2.com/nyaabucket/3a91454809f27e193887f7b5274de04b43ea40b943bb99ed6bd8f45b1b8da160/040-rm-pot-washers.jpg)

## Staring at the Chip

Here's what the chip looks like. This was a strange and very tall pot that I had
never seen before. After a bit of research, I found that it was a dual-gang
potentiometer. Essentially, that that means is that it's pretty much the same as
a normal potentiometer, but the same shaft controls 2 separate potentiometers
simultaenously, one for each ear.

![](https://s3.us-west-000.backblazeb2.com/nyaabucket/c8f410ab5da9be3c5d7fbd1c02ea4e2d0603e13310d15181ce33efb8ad5a8599/060-chip-top.jpg)

![](https://s3.us-west-000.backblazeb2.com/nyaabucket/6f8a05cd2b80b2718d951b93ec33eb1b244a9a042398c69e8c7458ee05ac9d05/060-chip-side.jpg)

![](https://s3.us-west-000.backblazeb2.com/nyaabucket/922b479d6c60895f594261d03051ff25e53c28217b365be9da6e814a937db262/060-chip-bottom.jpg)

I began making measurements on it, to make sure that it was working.

![](https://s3.us-west-000.backblazeb2.com/nyaabucket/7558d153955e447dad043d6187a22adcfc67a1b3237f498b975526ecdc140f7f/070-measurement.jpg)

As advertised, this is was a $20k\Omega$ potentiometer, give or take a
$k\Omega$. However, when I tried measuring the resistance of the middle leads, I
noticed that the bottom one changed resistance as I turned the potentiometer,
but the top one didn't. I thought I was maybe measuring it wrong, or my leads
were broken, so I swapped leads and noticeD the same thing. Very strange.

Rotary potentiometers have a wiper inside that moves when you spin the shaft,
controlling the resistance. I stared inside, and saw that the bottom one moved
as expected. However, I didn't even see a wiper for the top one!

I thought my eyes were deceiving me, so I desoldered and pried open the
potentiometer. And sure enough, the top gang was missing its wiper!

![](https://s3.us-west-000.backblazeb2.com/nyaabucket/43dd1c0daff59f88267922cf421f0ac8d81fe54d2fcafc95a89830ada82beb0e/090-pot-top.jpg)

![](https://s3.us-west-000.backblazeb2.com/nyaabucket/22df33c5f6e5864a2f903b7392d6b662ede9a6e8440a25d25385fd6bd0731e09/080-pot-open.jpg)

It seems I'll need to replace this potentiometer. I desoldered the rest of it
off, and ordered a new one from online.

![](https://s3.us-west-000.backblazeb2.com/nyaabucket/0c0a8f0399e188b0f70301e4b6a27749e3ff776c231fcd87ee316f9701a7191d/100-remove-pot.jpg)

## Conclusion

My initial guesses were both way off the mark. It was something even more basic,
even more fundamental than some component somewhere frying: it was a mechanical
issue inside the potentiometer.

I'll follow up with a post about the repair when the new potentiometer arrives.

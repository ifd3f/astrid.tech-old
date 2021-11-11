---
title: Misadventures in configuring an AT-D878UV radio
date: 2021-11-10 20:51:11-08:00
ordinal: 0
tags:
  - ham-radio
  - at-d878uv
  - uv-5r
  - w6bhz
  - linux
  - incompetency
---

Last week, because my Baofeng UV-5R's VHF band appeared to be broken and I
wanted to experiment with APRS, I decided to splurge on a Anytone AT-D878UV
radio. I received it from eBay today, and I was super eager to try it out!

However, I ended up encountering a whole bunch of stupid issues on the way
there, and spent approxmiately 5 hours trying to figure out what went wrong. So,
here's an article about my incompetency while doing that.

<!-- excerpt -->

## "Get a new radio," they said, "it would be fun," they said...

I wanted to at least be able to transmit to the W6BHZ repeater, so I wanted
those frequencies stored in memory. The radio does have a proprietary
Windows-only code-uploading software. Although my machine _is_ Linux/Windows
dual-boot, I was too lazy to switch into Windows. Instead, I tried using
[dmrconfig](https://github.com/OpenRTX/dmrconfig) and
[qdmr](https://dm3mat.darc.de/qdmr/) for that. So I plugged in my transceiver
and it turns out, they _were_ able to write the frequencies I wanted to the
radio!

When I tried transmitting a quick voice transmission, however, the radio sent a
carrier wave and nothing else. I confirmed with people on the repeater that they
received the carrier but no voice. Weird, but perhaps I can check it out at the
shack.

Okay, so if I can't transmit on memory, perhaps I configured it wrong. There is
a standard mode on all amateur radios that lets you arbitrarily tune into a
frequency and transmit or receive on it. This is called _VFO_, or Variable
Frequency Oscillation. It's a pretty important feature, so I wanted to change
into that mode to transmit. So I looked online at guides that told you how to do
it, and they were all like "Oh, go to Menu > Settings > Whatever and click on
the menu option." Unfortunately for me, my radio's menu seemed to have _way_
less options than they said there were.

Welp, I can probably get this all fixed at the shack!

## Enabling VFO using the official software

At the shack, I installed
[the official codeplug software (CPS)](http://www.wouxun.us/category.php?category_id=93)
on one of the machines running Windows. Using that software, I bound one of the
hotkeys to VFO, as instructed by
[this video from BridgeCom Systems](https://www.youtube.com/watch?v=K0wfUSmv-Jo).
Then, I uploaded it to my transceiver, pressed the keys, and... no mode change
happened.

Perhaps I didn't bind it to a valid key. I tried binding it to the key for
checking battery voltage, re-uploading it, and when I pressed that button, it
didn't even show voltage anymore! What happened?

Jack (KK6YWG) looked at the settings, and noticed that the radio mode was set to
**professional** rather than **amateur**, which was preventing me from using
VFO. As it turns out, this radio is not only designed for amateur use, but
commercial use as well, where they hand these radios to people who don't know
much about radios, and restrict a lot of settings (including, and _especially_,
VFO) so they don't accidentally transmit on the wrong frequencies. Once it was
turned to amateur mode, I finally had all the settings I was missing!

## The no-voice issue magically starts working

However, that still didn't solve the no-voice issue. Why couldn't any of my
transmissions get through? I tried more configurations, but I had no luck.
Someone else needed to use the computer for something, so I unplugged my radio
and decided to try flipping some settings inside the radio manually.

While doing that, my Baofeng was suddenly able to transmit on VHF again, so
technically I didn't _need_ to buy a new radio in the first place, but ah well,
I've gone too far to go back now.

But then, my Anytone was also able to transmit! I had no idea what the hell I
did, but whatever I did, it made it work. I decided to not touch the settings
until I got home.

## But it's doing that no-voice thing again!

Once I got home, I plugged in my radio and ran `dmrconfig -r` to back up my
working settings to a .img file of the working codeplug. I tried adding more
things to my radio using qdmr... but then my radio started randomly
transmitting, as if a ghost was pressing the push-to-talk every 6 seconds! It
repeatedly keyed the repeater that way, and I had to take off the antenna to
prevent it from clogging up the frequency.

Turns out that was because qdmr's codeplug file had VOX set to 775 rather than 0
(for off) for some strange reason. I fixed that in the YAML file, uploaded my
codeplug again, and it stopped spuriously transmitting.

Then, I tried transmitting my own voice... and it sent a carrier and no voice
again. What the _fuck_. I tried factory resetting the transceiver and uploading
the backup codeplug again. Still no luck. Also, I wasn't even able to _receive_
any transmissions either! It said it was receiving, but there was no sound! Even
when I held the monitor button, there was no sound!

## Turns out I'm a fucking idiot

Here I was, reconsidering my life choices, wondering if it was really the best
idea to buy a used AT-D878UV off of eBay, worrying that perhaps the speaker and
mic were busted, when I had a realization.

The programming cable goes into the same ports an external headphone or
microphone would.

So, I unplugged my radio...

...transmitted...

...and I got a 5-by-5 response.

The radio thought the USB cable was a headphone and microphone.

## Conclusion

Don't be stupid like me.

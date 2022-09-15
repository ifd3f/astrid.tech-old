---
title: Can we turn smartphones into fax machines?
date: 2021-10-21 22:51:53-07:00
ordinal: 0
tags:
  - research-notes
  - fax
  - w6bhz
  - ham-radio
---

At today's [CPARC](https://www.w6bhz.org/) meeting, I had an extremely stupid
idea. If fax is just audio over phone, what if you had a smartphone app that:

1. converted a PDF into a fax signal
2. dialed a fax machine
3. sent the fax signal over the phone call?

But then it gets better. What if on the receiving end, it wasn't a fax machine,
but another phone? So that phone

1. receives the phone call
2. receives the fax signal
3. decodes the fax into PDF

This seems like a completely awful idea, so much so that I kinda want to do it.
Some preliminary research I've done:

- There are so-called "fax sending apps" for Android phones, but those are lame.
  They aren't doing the actual dialing, transmission, and decoding; they're just
  calling some API on someone else's machine to do it for them.
- ["Group 3," or "G3"](https://en.wikipedia.org/wiki/Fax#Typical_characteristics)
  is either a machine or a mimetype. I'm not too sure which it is.
  - [Gough Lui](https://goughlui.com/project-fax/fax-technicalities-audio-samples/)
    has some information about the protocol.
  - [Here is a document from 1993 detailing the protocol](https://www.etsi.org/deliver/etsi_gts/03/0346/03.02.01_60/gsmts_0346sv030201p.pdf)?
- According to
  [Techopedia](https://www.techopedia.com/definition/25710/group-3-protocols),
  Group 3 protocols are the protocol? But then they also mention something about
  T.30?
  - [T.30 covers the five phases of faxing](https://www.dialogic.com/webhelp/msp1010/10.2.3/webhelp/MSP_DG/DSP_Info/fax_o.htm),
    so it might be what I want.
    - Setup
    - Select communication mode
    - Transmit message
    - Post-message processing/EOF/Confirm
    - Disconnect
- [Radiofax](https://en.wikipedia.org/wiki/Radiofax) is a thing. Fax is kinda
  like SSTV but with multiple pages.
- [Oh hey, there's a StackOverflow page about this.](https://stackoverflow.com/questions/5074128/how-to-implement-a-fax-protocol)
  - It says T.4 and T.30 are the protocols I want.
- Based on
  [this NetworkWorld article](https://www.networkworld.com/article/2346683/the-roles-of-t-30--t-4--and-t-6-in-fax-communications.html),
  T.4, T.6, and T.30 are the protocols I want. I suppose G3 is just the
  descriptor for the machine, and Group 3 protocols are those protocols.
  - Sadly it has no images,
    [even on the earliest archive](https://web.archive.org/web/20210126075809/https://www.networkworld.com/article/2346683/the-roles-of-t-30--t-4--and-t-6-in-fax-communications.html).
  - T.30 is the whole enchilada: handshake, negotiation, sending...
  - T.4 is about fax pages (MH/Modified Huffman and MR/Modified Read) and T.6
    (MMR/Modified Modified Read lmao)
- So we do want
  [the ITU's T.30 specification](https://www.itu.int/rec/T-REC-T.30-200509-I/en),
  with PDFs in multiple languages. Unfortunately, these are extremely long (322
  pages!)
- But then
  [based on this Superuser page, image/g3fax](https://superuser.com/questions/217785/how-to-convert-audio-file-of-fax-transmission-to-image-in-linux)
  is what I want because that's the whole recording of the fax?
  - However, fax is a two-way protocol with negotiation, so if I simply
    implement a .g3 decoder then I can't make a software fax machine.
  - And there's
    [a link back to Gough Lui](https://goughlui.com/2013/02/13/sounds-of-fax-modes-and-ecm/)
    with sample fax audios.
- [T.38 is a protocol for fax over IP](https://en.wikipedia.org/wiki/T.38). Not
  exactly what we want... is it?
  - This protocol is essentially a way to wrap a T.30 fax call into TCP and UDP
    packets while also sort of "adapting" the protocol around it into something
    suited for the internet. Like VoIP but for Fax. Thus, it's called FoIP.
  - Part of the protocol "is to 'fool' the terminal into 'thinking' that it's
    communicating directly with another T.30 terminal" in the event of packet
    loss?
- [HylaFAX+](https://hylafax.sourceforge.io/t30.php) is an open source T30
  thing???
  - It's written in C++, that's something I know!
  - The author mentions something about
    [Class 1 soft-modems](https://hylafax.sourceforge.io/howto/intro.php#ss1.4)?
  - [Nooooooo, Hylafax is only T.38, not T.30 and it wants real modems, not software!](https://stackoverflow.com/a/601256)
- A lot of fax libraries seem to cost money. That sucks.

Ugh, it's looking like I just _have_ to implement my own soft fax library...

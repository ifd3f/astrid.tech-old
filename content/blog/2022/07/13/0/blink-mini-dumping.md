---
title: "Blink Mini RE, Part 2 -- Desoldering and dumping the ROM"
description:
  I desoldered and resoldered a chip without legs, so I'm officially an expert
  on all things electronics
date: 2022-07-12 17:47:14-07:00
ordinal: 0
tags:
  - /projects/blink-mini-re
  - electrical-engineering
  - ghidra
  - reverse-engineering
  - soldering
  - incompetency
---

![The Blink Mini board, focused on the ROM.](https://astrid.tech/_/2022/07/07/0/focus-rom.jpg)

This is a continuation from
[the post where I disassembled the lil guy](https://astrid.tech/2022/07/07/0/blink-mini-disassembly/).

Last time, I couldn't do anything about this chip because I was waiting for my
SPI flash programmer kit to arrive.

![The chip, from the side view.](https://s3.us-west-000.backblazeb2.com/nyaabucket/b1d6f05faeb930ae1154c78b42eddbc5f5f824b4a5cbbffa579e1fc74785385e/000-chip-prof-cropped.jpg)

On Saturday, though, it arrived!

## Trying to use the programmer kit on Saturday

The programmer kit came with a bunch of parts for a bunch of different
scenarios. Of course, it included the SOIC-8 clip that I wanted.

![The assembled programmer kit, top view.](https://s3.us-west-000.backblazeb2.com/nyaabucket/332a6acf6a4865ece24f20e0e023e0b78eab96759947b6f37276b915e9402ef0/001-prog-top.jpg)

![The assembled programmer kit, side view.](https://s3.us-west-000.backblazeb2.com/nyaabucket/b153fd333984d84c091b139b26c35d8c7599ce03dcbadb7cc3606b9bf81983df/001-prog-prof.jpg)

From bottom to top, these are the components shown here, assembled together:

1. The SPI programmer, a USB dongle with a ZIF connector on it.
2. A 1.8V adapter, because the programmer is 5V/3.3V, but the flash is 1.8V
   according to the datasheet.
3. The adapter for the SOIC clip.

### Clipping onto the ROM, peacefully

I put the SOIC clip in the slot at the top, and tried sticking it on the chip.
Sadly though, running `flashrom` to dump the firmware, I got

```text
# flashrom --programmer ch341a_spi -r flash.bin
flashrom v1.2 on Linux 5.15.49 (x86_64)
flashrom is free software, get the source code at https://flashrom.org

Using clock_gettime for delay loops (clk_id: 1, resolution: 1ns).
No EEPROM/flash device found.
Note: flashrom can never write if the flash chip isn't found automatically.
```

Turns out, the clip leads were _way_ too far away from the flash leads, as you
can see with the circled parts of this image.

![The SOIC clip, unable to reach the leads of the ROM.](https://s3.us-west-000.backblazeb2.com/nyaabucket/af6a69702d0444a7a4857866443e3116e3a591ac1332f479ab215d210850d167/002-clipping-fail-anno.jpg)

This was unfortunate, but it made sense; this ROM is in a WSON-8 package rather
than a SOIC-8 package, which the clip was designed for. You can see from the
image that there's even little plastic guards that are there to protect or space
out the leads.

### Clipping onto the ROM, with great violence

As such, I decided to make some "special modifications" to the clip using a wire
cutter.

![SOIC clip, with one of the little plastic teeth broken off.](https://s3.us-west-000.backblazeb2.com/nyaabucket/3f04a92252b523ecd6bc2bde490f114dcb5e824d1300ede4b16f52ccd9d3f594/100-break.jpg)

![SOIC clip, with all of the little plastic teeth broken off, from the top down.](https://s3.us-west-000.backblazeb2.com/nyaabucket/c08ecc5ea17d9a6cfce394b38839340b6101a45caa18dcb53169726af9d85d9d/101-top-down.jpg)

This still wasn't enough!

```text
# flashrom --programmer ch341a_spi -r flash.bin
flashrom v1.2 on Linux 5.15.49 (x86_64)
flashrom is free software, get the source code at https://flashrom.org

Using clock_gettime for delay loops (clk_id: 1, resolution: 1ns).
No EEPROM/flash device found.
Note: flashrom can never write if the flash chip isn't found automatically.
```

My solution? I trimmed even more off! Additionally, the spring was too strong,
so I experimented with putting a pen spring inside.

![SOIC clip with lots of plastic removed, and the leads trimmed, and the spring removed.](https://s3.us-west-000.backblazeb2.com/nyaabucket/0c6a6921b04d5a5f5c7f45c4623d968ba94448ff2647c4d838cf90fc4ad5a2eb/102-spring-out.jpg)

![A retractable ballpoint pen from University of Tulsa that I'm disassembling.](https://s3.us-west-000.backblazeb2.com/nyaabucket/78707f72021ba3a8b7e87060a90d1589f5c1b4b0d3cc8d704b7a01a673de28bf/103-pen.jpg)

![The long, thin, petite spring from that pen next to the huge, thick, chonky SOIC clip spring.](https://s3.us-west-000.backblazeb2.com/nyaabucket/a4cbce764f083834d2ff74fa6cf43269cb2051f1d85e596fd2d173b0839df68d/104-pen-disasm.jpg)

Unfortunately, that pen spring was way too long, but on the bright side, having
the spring be partially placed in provided just enough force for it to stay
there.

![SOIC clip, with one of the sides of the spring inside the hole, and the other side not inside the hole.](https://s3.us-west-000.backblazeb2.com/nyaabucket/1a33e22ac2ce0463be70ecec80894fbcccb98b63f06acc716997c8e849b6dfd9/105-less-force-spring.jpg)

Still, even with these modifications, nothing worked, I just couldn't maintain
continuity for long enough to allow the programmer dongle to find the chip,
probably because my janky solution was just too janky.

### Ordering _yet another_ clip 😔

I couldn't figure out anything else I could do that Saturday night. There are
proper probes for programming WSON-8, essentially based on pogo pins.
Unfortunately, they were expensive and
[only available on AliExpress](https://www.aliexpress.com/item/3256804113101513.html),
and I didn't want to wait a month for it to arrive. I still put in an order for
them though, because I was optimistic that I'd be able to reverse engineer it
and reprogram my other Blink cameras with my own firmware.

![The proper pogo probes for WSON-8 chips.](https://s3.us-west-000.backblazeb2.com/nyaabucket/4e9959cec55d77e58e60c7c2985d1642fd28cb14e614154e392c126f109d6110/pogo-wson.png)

I wanted to avoid it, but this stupid SMD part has forced my hand; I'd have to
go to the EE lab and desolder it.

## Sunday Soldering Fun

I was extremely excited to desolder that ROM. However, on the Lord's day, you're
supposed to rest. Fortunately, I'm not religious at all, so I actually got up
early to get dressed, eat breakfast, and bike over to campus. (I never do this!)

The EE department, however, seemed to disagree with my disagreement; the doors
to the EE building and student project lab were locked, because it was a Sunday,
and in the middle of the summer. Thankfully, I had a passcode to the
[ham radio](https://www.w6bhz.org/) clubroom, which did have a reflow station
that I could borrow.

![The reflow station in the ham radio clubroom.](https://s3.us-west-000.backblazeb2.com/nyaabucket/0f7542e0cfefe48ce572d5e54c3d0ae27e4b898340e60e242fd4e2a8cf552ba6/200-reflow-w6bhz.jpg)

Pulling the chip off was trivially easy with the hot air gun and tweezers.

![The desoldered ROM!](https://s3.us-west-000.backblazeb2.com/nyaabucket/288dfc34049c1f0ee00508079b728a8297f6f4cd66f2b247544339a6cfa5629b/201-desolder.jpg)

![The PCB with our friend, the ROM, missing.](https://s3.us-west-000.backblazeb2.com/nyaabucket/0143ec48d4468201d08a406222fc4f2fc3f3687c6e46f6b19d80e1fd210adc14/202-pcb-no-chip.jpg)

![The now-removed flash chip, top and bottom.](https://s3.us-west-000.backblazeb2.com/nyaabucket/1dc40b5c185d340274852c8e68a5a8f55128fbd749ecd0414868f0ece852889d/203-chip-top-bottom.jpg)

The programmer kit was very nice, and it came with this little
breakout-board/adapter jig that I could solder my SMD part onto so that it would
fit in the ZIF connector.

![Top view of the breakout with the ROM soldered on.](https://s3.us-west-000.backblazeb2.com/nyaabucket/8c0a5f798686932aa8713114dfb8ee73f2e02463375c03553a7b559350518ef6/205-on-board.jpg)

Well, the headers were a bit slanted, but that's okay, it didn't matter, it fit
into the ZIF anyways.

![Side view of the breakout. My headers are slanted because I'm stupid.](https://s3.us-west-000.backblazeb2.com/nyaabucket/c258a5c89a9f4c458ef40539b38f8a39be683dc695a6afb99f93ec6c98a8cbe5/206-slanted-pins.jpg)

![The breakout board on top of the 1.8v adapter on top of the programmer](https://s3.us-west-000.backblazeb2.com/nyaabucket/b0772ec88f9cba82608b21e4dc83453f18723c74c0b7bd16384f75c06b13548e/207-on-programmer-2.jpg)

I plugged it into my computer, and...

```text
# flashrom --programmer ch341a_spi -r flash.bin
flashrom v1.2 on Linux 5.15.49 (x86_64)
flashrom is free software, get the source code at https://flashrom.org

Using clock_gettime for delay loops (clk_id: 1, resolution: 1ns).
No EEPROM/flash device found.
Note: flashrom can never write if the flash chip isn't found automatically.
```

Uh oh. I tried again.

```text
# flashrom --programmer ch341a_spi -r flash.bin
flashrom v1.2 on Linux 5.15.49 (x86_64)
flashrom is free software, get the source code at https://flashrom.org

Using clock_gettime for delay loops (clk_id: 1, resolution: 1ns).
No EEPROM/flash device found.
Note: flashrom can never write if the flash chip isn't found automatically.
```

### Panic!

It was at this point that I started panicking a little, trying to think what
could have gone wrong.

**Hypothesis 1 -- I electrically fried it.** I do remember, when I was finagling
with the SOIC-8 clip earlier, that I tried flipping the clip around. And, as you
can see from this pinout, the VCC and GND pins are rotationally opposite from
each other. In other words, I may have accidentally put it under the wrong
polarity.

![Pinout of the WSON-8 version of W25Q32JW. GND is at pin 4 and VCC is at pin 8.](https://s3.us-west-000.backblazeb2.com/nyaabucket/3cf6230e35c38daabba74976e16e979e62de6e071b52a87c938bccf395c71da6/w25q32jq-pinout.png)

**Hypothesis 2 -- I thermally fried it.** The datasheet stated that the chip's
lead/package temperature is "compliant with JEDEC Standard J-STD-20C for small
body Sn-Pb or Pb-free (Green) assembly and the European directive on
restrictions on hazardous substances (RoHS) 2002/95/EU." [^1] This means that it
is rated for a maximum package temperature that is somewhere between 240C and
260C, and probably unflashed, too. I had subjected it to 360C, because I usually
solder at 325-375F, and my brain forgot that units were a thing.

[^1]:
    [W25Q32JW datasheet](https://www.mouser.com/datasheet/2/949/w25q32jw_spi_revf_09042018-1489621.pdf)
    section 9.1 (page 57, PDF page 58)

At this point, I was a bit sad, but I did have more ~~potential victims~~
subjects that I could dump the ROM off of. I thought I should solder it back
onto the board and power it, just for _final_ final confirmation that it was
broken.

But then, Erin suggested that I should probably double check that my IC had
continuity with each pad. At this point, I had already taken it off and was
tinning the original Blink board, but then I was suddenly like "hold up, what if
it was just a shitty connection that was causing it?"

![The original soldering job. It was very bad and ugly.](https://s3.us-west-000.backblazeb2.com/nyaabucket/8989f76150242896d7235fa253600a1e5fdae43dac94dfe660247b511c855bbc/208-bad-soldering.jpg)

![The new soldering job. It is very nice and clean and, in fact, functional.](https://s3.us-west-000.backblazeb2.com/nyaabucket/285978dd90d93828b6a54a1fa75c5fcf8de15767ea682dccb9be37db2ead408f/209-reflowed.jpg)

Thus, on Sunday, June 10, 2022, at 11:07 AM PST, once I had reflowed the solder
joints and put my ROM chip back on, I had learned the value of disconnecting and
reconnecting things.

![Discord message, from Dark Absorbing Transistor — 07/10/2022: Guess I'll give it one more shot / Can't hurt / Oh fuck / Holy shit / It worked / Ahhhhjj](https://s3.us-west-000.backblazeb2.com/nyaabucket/57b8c2ed7da6da1659af3398e315e108a8432bfdf27d43fd34de0cfbe92c7c26/discord-msgs.png)

```text
# flashrom --programmer ch341a_spi -r flash.bin
flashrom v1.2 on Linux 5.15.49 (x86_64)
flashrom is free software, get the source code at https://flashrom.org

Using clock_gettime for delay loops (clk_id: 1, resolution: 1ns).
Found Winbond flash chip "W25Q32.W" (4096 kB, SPI) on ch341a_spi.
Reading flash... done.

# ls -lh flash.bin
-rw-r--r-- 1 root root 4.0M Jul 12 17:21 flash.bin
```

I ran it over with `strings` and `binwalk`, and there was a lot of good output,
too! This was the real deal!

```text
$ strings flash.bin # output truncated for brevity
usbssphy_sleep3=0x%x
leddc=0x%04x
mcs32po=0x%x
rssisav2g=%d
opo=%d
itt2ga1=0x%x
rxpo5g=%d
extpagain5g=0x%x
Blink1
admin@immediasemi.com0
u033
UWl1P-rXREldiicyWs61P0g
2866E200CBEA8F7D34FA6C288F236ABD91A601AF77BC170D662C3BC8873F0596WAP: Wireless Access Point
WAP: Wireless Access Point
Blink thread
../../src/blink/blink.c
G8V190001173FW7G
OWZC31E06A1CC
OWLC21E07FDC1

$ binwalk flash.bin
DECIMAL       HEXADECIMAL     DESCRIPTION
--------------------------------------------------------------------------------
325424        0x4F730         Base64 standard index table
325868        0x4F8EC         SHA256 hash constants, big endian
332080        0x51130         AES S-Box
332336        0x51230         AES Inverse S-Box
332604        0x5133C         DES SP1, big endian
332860        0x5143C         DES SP2, big endian
368048        0x59DB0         SHA256 hash constants, big endian
387744        0x5EAA0         Copyright string: "Copyright (c) 1996-2018 Express Logic Inc. * NetX Duo ARC600/MetaWare Version G5.11.5.0 SN: Amaxon_Eval_10-18-18 *"
797085        0xC299D         Certificate in DER format (x509 v3), header length: 4, sequence length: 101
888232        0xD8DA8         Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
889272        0xD91B8         Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
890312        0xD95C8         Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
891352        0xD99D8         Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
892392        0xD9DE8         Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
893432        0xDA1F8         Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
894472        0xDA608         Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
895512        0xDAA18         Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
896552        0xDAE28         Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
897592        0xDB238         Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
1203868       0x125E9C        Base64 standard index table
1204312       0x126058        SHA256 hash constants, big endian
1209308       0x1273DC        AES S-Box
1209564       0x1274DC        AES Inverse S-Box
1209832       0x1275E8        DES SP1, big endian
1210088       0x1276E8        DES SP2, big endian
1212316       0x127F9C        SHA256 hash constants, big endian
1231620       0x12CB04        Copyright string: "Copyright (c) 1996-2018 Express Logic Inc. * NetX Duo ARC600/MetaWare Version G5.11.5.0 SN: Amaxon_Eval_10-18-18 *"
1671037       0x197F7D        Certificate in DER format (x509 v3), header length: 4, sequence length: 101
1762184       0x1AE388        Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
1763224       0x1AE798        Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
1764264       0x1AEBA8        Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
1765304       0x1AEFB8        Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
1766344       0x1AF3C8        Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
1767384       0x1AF7D8        Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
1768424       0x1AFBE8        Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
1769464       0x1AFFF8        Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
1770504       0x1B0408        Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
1771544       0x1B0818        Certificate in DER format (x509 v3), header length: 4, sequence length: 1033
```

I was so happy that stopped by at the grocery store and bought some chips and
salsa to celebrate my glorious victory. My roommates have eaten most of it by
now.

## Beginning to analyze our binary!

Here's what the first 2MiB of the binary looks like in
[binvis](http://binvis.io/):

![Binviz output, by byteclass](https://s3.us-west-000.backblazeb2.com/nyaabucket/16d1ecb15abc4c211bfcb5ff1fd3b6f8d57fd0d352bd776c57172e9315e01ac6/binvis-byteclass.png)
![Binviz output, by entropy](https://s3.us-west-000.backblazeb2.com/nyaabucket/9b3b08e6e19ee98645c561d3e602e35a1d45c4789f468d108efbf473aeee8f41/binvis-entropy.png)

I haven't had much time to analyze this binary, but here's a few things I've
found so far from poking around with Python scripts and Ghidra:

- There are lots of empty/erased sectors on this flash; in fact, is true size
  seems to only be `0x1f0070`, or nearly 2MiB. The rest of the sectors are
  filled with the byte `0xFF`.
- Little-endian ARMv8 seems to have the best results with decompiling the image.
  I've found a function located at `0x7d664` that is almost definitely
  `memcpy(3)`, and is referenced by a ton of other functions nearby (thanks,
  relative jumps!)
- The WAP name is located at `0x1e02aa`
- It seems to have 2 slightly different copies of the firmware stored on board,
  implying they use a sort of A/B booting scheme so that they can fall back to a
  working copy in case of issues.
- There's some extremely rich logging here, even including source code paths
  (such as `../../src/asic/video_enc/h264_fw/enc_command.c`)

Sadly, there is a lot of strange memory mapping going on, so I have no idea
where the text and data is _supposed_ to go, and I don't even know what
functions are writing these logs!

I'll likely go into detail on how I figured these out in a next post. There are
so many possible next steps in terms of reverse engineering it, and it makes me
extremely excited! Unfortunately, there is also so much schoolwork for me to do
in the next two weeks, so I probably won't make any progress until I finally
complete these last GE classes and get my full CS degree.

## TL;DR of this post

I did it, I dumped it, I'm super smart, I'm super cool, I have a 2MiB firmware
image and it's ARM but I don't know what most of it means yet.

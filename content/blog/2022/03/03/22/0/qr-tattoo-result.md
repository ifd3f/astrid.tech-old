---
title: I now have a QR code tattoo!
date: 2022-03-03 07:48:02-08:00
ordinal: 0
tags:
  - /projects/qr-tattoo
  - rust
  - transhumanism
---

Following up from [my last post about temporary tattoos](https://astrid.tech/2021/10/03/0/temp-tattoo-results/), I finally got it permanently etched onto my skin! Thanks to Kelsey Kansas at [Crybaby Tattoo](https://crybabytattoo.square.site/) in San Luis Obispo!

And yes, *it can be scanned.*

![The permanent tattoo, a day after it was drawn](./fewdays.jpg)

## The process

This was my first tattoo ever, so I wasn't completely sure of what to expect besides pain, and lots of it. My appointment was on a warm Sunday afternoon, on February 27.

As it turns out, the tattoo artist doesn't just freedraw the entire tattoo onto your skin, which makes sense, since that seems like it would be very prone to errors. Instead, Kelsey had a specialized stencil printer that

![The traced outline of the tattoo.](./traced.jpg)

![Partially outlined tattoo.](./outlining.jpg)

![The tattoo right after Kelsey finished drawing it.](./complete.jpg)

## Tattoo Controller Server

The tattoo actually points to a very basic web service that I wrote in Rust to perform the on-demand redirection. I actually wrote it on 2021-11-17 well in advance of actually getting the tattoo. There is a very barebones admin panel I wrote that lets me paste in a URL and select where I want it to go.

![A screenshot of the admin panel.](/projects/qr-tattoo/admin.jpg)

More details on the implementation can be found [on the project page](/projects/qr-tattoo).


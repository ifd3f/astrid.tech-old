---
title: Skeleton Stick, a hardware password manager
date: 2022-03-10 09:45:08-08:00
ordinal: 0
tags:
  - raspberry-pi
  - python
  - cybersecurity
  - /projects/skeleton-stick
---

I got to choose any security-related project I wanted for my CPE 321 (Intro to
Cybersecurity) final project. So, I chose to make a hardware password manager on
a Raspberry Pi!

![The password manager, unlocked](/_/projects/unlocked.jpg)

<!-- excerpt -->

[Check out the project page for more details on its design, and a link to the design document I wrote about it for class.](/projects/skeleton-stick)

I implemented it in a total of about 15 hours. It was a very interesting project
covering lots of topics:

- **USB Human Interface Devices (HID)** - I had to pore over
  [the USB HID spec](https://usb.org/sites/default/files/hut1_3_0.pdf) to
  implement the password keying.
- **SystemD and Linux Init** - I learned about the different targets and where
  to stick my own services. I also researched ways to implement my own init
  process for a v2 of this device.
- **Encryption and Key Derivation** - I learned more about bcrypt and AES and
  how to store stuff securely.
- **Hardware and physical security** - It turns out that hardware security is
  kinda hard and there's lots of ways to hack this device!

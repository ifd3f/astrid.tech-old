---
title: Why do I put stuff in monorepos?
date: 2021-10-07 16:39:59-07:00
ordinal: 0
tags:
  - git
---

Monorepos are great, and here's why like to use them for my personal projects.

<!-- excerpt -->

## What is a monorepo?

If you have a project with multiple services, like a frontend and backend, you
can either put them in separate Git repos, or put them in the same repo but in
different folders.
[The source code for my website](https://github.com/ifd3f/astrid.tech)
is an example of a monorepo with 6 subprojects (specifically, aay_tw_shortener/,
astrid_tech_api/, astrid_tech_frontend/, content/, wm-receiver/, and scripts/)

```
❯ tree -aFL 1 astrid.tech
astrid.tech
├── aay_tw_shortener/
├── astrid_tech_api/
├── astrid_tech_frontend/
├── Cargo.lock
├── Cargo.toml
├── content/
├── .git/
├── .gitattributes
├── .github/
├── .gitignore
├── lc.sh*
├── LICENSE
├── quickpost.sh*
├── README.md
├── scripts/
├── target/
└── wm-receiver/

9 directories, 8 files
```

Google, Facebook, Microsoft, and Uber are some of the biggest monorepo users
currently.

## Why do this?

### Less-cluttered Github account

At time of writing, [my Github account](https://github.com/ifd3f) has
75 repos of varying sizes. Using monorepos reduces the administrative overhead,
and helps group related items together.

As an example, I have a monorepo called
[coursework](https://github.com/ifd3f/coursework), which contains
(almost) all my coursework and notes since 2018. It's private, so you can't see
it, but every class I've taken gets its own folder:

```
❯ tree -aFL 1 coursework
coursework
├── 2018-01_CSM-cpp-class/
├── 2018-08_CSM-data-structures/
├── 2019-09_CPE202/
├── 2020-01_CPE133/
├── 2020-01_STAT312/
├── 2020-04_CPE233/
├── 2020-04_CSC348/
├── 2020-04_EE143/
├── 2020-06_CPE357/
├── 2020-09_CSC572/
├── 2020-09_ENGL149/
├── 2020-09_PHYS132/
├── 2021-01_CPE333/
├── 2021-01_CSC307/
├── 2021-09_ANT202/
├── 2021-09_BIO213/
├── 2021-09_BMED213/
├── 2021-09_CSC349/
├── 2021-09_MATH335/
├── default.nix
├── .git/
├── .gitattributes
├── .gitignore
├── .gitmodules
├── Makefile
└── README.md

20 directories, 6 files
```

It's a bit tedious to create a new repo for every class, and oftentimes, I will
refer to coursework I've done in earlier years for my current coursework. Thus,
this is a great solution for me.

### Nicer to develop with

When making an app with multiple services (like a frontend, backend, another
backend service...) there's no getting around the fact that the backend and the
frontend depend on each other a lot.

In the past, when I wrote
[Collision Zone](https://github.com/ifd3f/collision-zone.git), I had
never written a project that used multiple programming languages or had multiple
webservices.[^1] So, my first instinct was to use a polyrepo setup, where
Node.js/TypeScript goes in one repo containing matchmaking code and frontend,
and the C++ game server code goes in the other. However, what often ended up
happening was that I would make a change to the protocol on one end, then change
the protocol on the other end, so I ended up making 2 commits in different
repos. It got very tedious very quickly. Additionally, my frontend and backend
code would have also been at risk of becoming out-of-sync in terms of commits
while deploying it. This was not something I actually encountered, but at some
point it would have been likely to happen.

On the other hand,
[astrid.tech](https://github.com/ifd3f/astrid.tech), the source code
for this website, is a monorepo setup. If I decide I want to change my API, I
change the backend and frontend in a single commit.

Additionally, with the coursework repo, there's less copy-pasting of .gitignore
and .gitattributes files. I can use the same ignores and LFS configs across all
my classes.

### When similar projects are too small to deserve their own repo

I have a [memes](https://github.com/ifd3f/memes) repo, for my
open-source memes:

```
❯ tree -aFL 1 memes
memes
├── brain-meme/
├── disaster/
├── .git/
├── .gitattributes
├── .gitignore
├── integer-bhj/
├── LICENSE
├── Makefile
├── ratjam/
└── README.md

5 directories, 5 files
```

Each meme is too small to deserve its own repo.

Slightly more useful, I have a
[Minecraft-Computers](https://github.com/ifd3f/Minecraft-Computers)
repo containing code for programming in-game ComputerCraft and OpenComputers
computers:

```
❯ tree -F Minecraft-Computers
Minecraft-Computers
├── computercraft/
│   ├── password.lua*
│   └── reactor.lua*
└── opencomputers/
    ├── agricraft.lua*
    ├── deadreckoning.lua*
    ├── islandexp.lua*
    ├── oresort3.lua*
    ├── pidreactor.lua*
    ├── puredaisy.lua*
    ├── quarry.lua*
    └── reactor.lua*

2 directories, 10 files
```

You may notice that, at time of writing, this repo only has a single commit.
That's because before, they weren't checked into Git, and I thought "yeah it's
too small for its own repo" and I ended up not versioning those files.

Okay, maybe both of those projects are kinda silly or not useful. This is my
[infra](https://github.com/ifd3f/infra) repo, which is essentially a
big repo for any kind of configuration under the sun. It deploys this website as
well as other services, and I'm even in the middle of merging my dotfiles into
this repo, too.

```
❯ tree -FL 1 infra
infra
├── ansible/
├── astral-cloud.code-workspace
├── dev-env/
├── docker-compose/
├── docs/
├── flake.lock
├── flake.nix
├── home-manager/
├── images/
├── kubernetes/
├── marttab
├── nixos/
├── openwrt/
├── pull-dotfiles.sh*
├── README.md
├── scripts/
├── ssh_keys/
└── terraform/
```

This repo configures so many different kinds of services and apps, but
oftentimes, individual services are too small to warrant their own individual
repo. The Ansible playbooks are relatively distinct, but I definitely would not
want to individually put them in their own repos.

## Why not have one big ~~union~~ monorepo for everything, like Google does?

Unlike Google, I don't consolidate all of my everything into one gigantic
monorepo, but I do consolidate related items into topical or project monorepos.
I guess the line is a bit hard to pin down, but if I'd consider them
closely-related enough, then I would put them in a monorepo. For example, I
wouldn't put memes in the same repo as coursework. One is public and the other
is private, plus there aren't really cross-cutting concerns and I wouldn't be
using the files in memes for related activities in coursework. I wouldn't put 2
completely different games in the same repo, either.

However, there is a good argument to be made for uniting the astrid.tech and
infra repos. It turns out deployment is actually somewhat coupled with code, and
I've found myself doing the same things I did with Collision Zone (make a change
in one repo, make a related change in the other). However, I have them separate
for now, because astrid.tech is sort of focused on code, while infra is sort of
focused on configuration. Facebook internally has a similar split, with backend
code in one repo and the deployment configurations in another. However, I
believe the reason for that is more of a technical one: they haven't developed
Mercurial enough to handle having both in the same repo. If Mercurial could
support it, they probably would do it.

[^1]:
    You can definitely see my inexperience in the architecture of that project.
    For example, I did have a SPA frontend, but then I had the Node.js server
    serving that instead of nginx. I also didn't know anything about Docker
    (which is why the code no longer works if you run that repo).

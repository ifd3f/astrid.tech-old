---
title: I used NixOS for a week
date: 2021-10-09 14:44:58-07:00
ordinal: 0
tags:
  - nixos
  - linux
---

I spent the week using a travel laptop with NixOS installed on it while at
school. On that laptop, I fully bought into the NixOS ecosystem, including
managing my /home folder + dotfiles using home-manager.[^1] Here's my experience
with it so far.

<!-- excerpt -->

[^1]:
    All my configs are contained within the
    [infra](https://github.com/ifd3f/infra) repo.

## Dotfiles

Tracking my i3wm in home-manager was slightly unwieldy at first, but I'm getting
the hang of it. Home manager actually has a sort of i3 "templater" that lets you
do tricks like this, which binds both HJKL and arrow keys to the same features,
allowing me to [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself):

```nix
let
  forEachL = lib.forEach [ "h" "Left" ];
  forEachD = lib.forEach [ "j" "Down" ];
  forEachU = lib.forEach [ "k" "Up" ];
  forEachR = lib.forEach [ "l" "Right" ];

  # Usage: forEachDirKey (d: k: { "${mod}+${k}" = "focus ${d}"; });
  forEachDirKey = f:
    (forEachL (f "left")) ++ (forEachD (f "down")) ++ (forEachU (f "up"))
    ++ (forEachR (f "right"));
in focusDirBinds = forEachDirKey (d: k: { "${mod}+${k}" = "focus ${d}"; });
```

## It just works (mostly so far)

Most of the software I use has worked without a hitch. This includes TeXStudio,
Visual Studio Code, Neovim, Firefox, GitKraken, and more. Discord is a bit
laggy, though, and I'll be looking into that.

## Declarative and Explicit

It's super nice being able to declaratively and explicitly say which packages
and configurations I want to include on the machine. On my main computer, which
is running Arch right now, I'm experiencing _all_ the configuration drift. I've
lost track of...

- ...what programs are installed in what package managers? Keep in mind that at
  one point, Linuxbrew and Snap were on this list as well.

```
❯ neofetch
# -snip- #
             `/:-:++oooo+:               Packages: 1849 (pacman), 79 (nix-user), 9 (flatpak)
```

- ...what programs are installed _outside_ of package managers? (i.e.
  /usr/local/bin or /opt or
  [/usr/local/opt](https://stackoverflow.com/questions/35337601/why-is-there-a-usr-local-opt-directory-created-by-homebrew-and-should-i-use-it))
- ...what hacks have I done in /etc to get things working? I have etckeeper
  installed but I (admittedly) have not been maintaining discipline with
  explicitly committing whenever I change /etc.
- ...what configs do I have in ~astrid? I have yadm installed to check in _some_
  files, but that /home has gone from Ubuntu to another Ubuntu to Arch Linux,
  and probably has so many _things_ littered around it that I'm afraid to delete
  stuff.

I'm considering starting the Arch machine afresh with NixOS, or at least have my
Arch dotfiles managed by home-manager. I'll probably end up trying both.

## Caveats

Not everything has gone perfectly well, though.

- As mentioned earlier, Discord is laggy for no good reason.
- I haven't done much programming on my NixOS machine just yet, only editing
  $\LaTeX$ documents.

I'm still enjoying it though, and I'll definitely continue using it.

---
title: Using PRs in Nixpkgs before they actually land in Nixpkgs
description: A guide on using overlays to enable your impatience
date: 2022-11-02 18:44:56-07:00
ordinal: 0
tags:
  - nixos
  - fediverse
---

Nixpkgs is a very rich package repository, but sometimes it doesn't have the
package that you want. However, if you're lucky, someone else might be working
on adding it right this instant! You can check
[the current pending pull requests](https://github.com/NixOS/nixpkgs/pulls) to
confirm this.

There's a good chance that their version works; perhaps with some little bugs or
kinks, but it still works. If you used it, you could even submit feedback about
how well it works! However, since it hasn't gotten into nixpkgs yet, you'll need
to overlay it in order to use it. That's what I'll be explaining in this
article.

The example I'll be using as an example
[PR #192285](https://github.com/NixOS/nixpkgs/pull/192285), which adds
[Akkoma](https://akkoma.dev), a social networking software that is part of the
[Fediverse](https://en.wikipedia.org/wiki/Fediverse). At time of writing, it
still hasn't landed in nixpkgs, but I wanted to spin up my own Akkoma server, so
I did this process to include it.

This guide will be flake-focused, but it can easily be adapted to non-flake code
by using `pkgs.fetchgit` or `builtins.fetchGit` as necessary.

## Downloading the PR

From the PR page, you can click on the repo that the PR's code comes from. In
this case, it comes from the repo
[`illdefined/nixpkgs`, branch `akkoma`](https://github.com/illdefined/nixpkgs/tree/akkoma).

![The PR page, indicating a merge request from illdefined/nixpkgs, branch akkoma into nixpkgs](https://nyaabucket.s3.us-west-000.backblazeb2.com/a303055ac41808372234f30bfb15844820f51af9c38a6ae625678eb5f94a0922/nixpkgs-akkoma-pr-info.png)

So, we'll fetch that repo into our code. If you're using a flake, you can just
add the input like so:

```nix
{
  inputs = {
    # Your regular nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # The PR's repo
    nixpkgs-akkoma.url = "github:illdefined/nixpkgs/akkoma";

    ...
  };
}
```

## Overlaying the packages

Now that that's done, you'll need to create an overlay for nixpkgs and apply it
to your base nixpkgs.

You might be wondering, why do we want to use an overlay instead of just
pointing our nixpkgs to `nixpkgs-akkoma`? There are a few reasons:

- Their feature branch is most likely not as updated as the base nixpkgs. You
  won't be getting updates for your other packages if you just point your
  nixpkgs to the PR branch.
- Creating an overlay lets you apply changes from multiple PRs!

To build our overlay, we'll want to find out what packages that the patch added
or changed. You can usually see this in the diff for
`pkgs/top-level/all-packages.nix`, which is basically what you get when you call
`import nixpkgs {}`.

![The diff of all-packages.nix, showing the addition of attributes `akkoma` and `akkoma-frontends`](https://nyaabucket.s3.us-west-000.backblazeb2.com/0e2121492b1d0310d3ef4ca32cbc2be86ebab46dab71c80cc6d511bfc39392f1/all-packages-diff.png)

In our case, 2 attributes were added: `akkoma` and `akkoma-frontends`. We will
add this to an overlay and apply it to our base nixpkgs like so:

```nix
let
  # Our customized overlay
  overlay = final: prev: {
    # Inherit the changes into the overlay
    inherit (nixpkgs-akkoma.legacyPackages.${prev.system})
      akkoma akkoma-frontends;
  };

  # the pkgs that you use for everything else
  pkgs = import nixpkgs {
    system = "<your system here>";
    overlays = [ overlay ];
  };
in { ... }
```

Now, the result of `pkgs.akkoma` and `pkgs.akkoma-frontends` will be whatever
the PR has for it.

## Using it in NixOS

If we were doing a standdalone Nix project that didn't involve NixOS whatsoever,
we would be done. However, if we wanted to use the PR's provided NixOS modules,
it would not immediately work because the modules it added were not added to our
base nixpkgs.

The added modules will be found in `nixos/modules/module-list.nix`.

![The diff of `module-list.nix`, showing the addition of `./services/web-apps/akkoma.nix`](https://nyaabucket.s3.us-west-000.backblazeb2.com/4ee7a5750c298afc93c173f81666d1b091d3e9af786c75dddbbaeece7a04ddda/module-list-diff.png)

So, we'll just need to directly import the PR's module in our own module like
so, and we can use the PR's NixOS changes!

```nix
{
  imports = [
    "${nixpkgs-akkoma}/nixos/modules/services/web-apps/akkoma.nix"
  ];

  nixpkgs.overlays = [ overlay ]; # the one from earlier

  # An option that the PR added
  services.akkoma.enable = true;
}
```

## Adding another PR on

Remember how I mentioned that this technique lets you combine multiple PRs?
Let's say we also wanted to include a repo that adds a program called
`foobarspam`. We just add the new input:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-akkoma.url = "github:illdefined/nixpkgs/akkoma";
    nixpkgs-foobarspam.url = "<foobarspam's repo would go here>";
  };
}
```

... inherit more attributes into our overlay:

```nix
final: prev: {
  inherit (nixpkgs-akkoma.legacyPackages.${prev.system})
    akkoma akkoma-frontends;

  inherit (nixpkgs-foobarspam.legacyPackages.${prev.system})
    foobarspam-server foobarspam-cli foobarspam-lib;
}
```

... and finally, import additional NixOS modules as necessary:

```nix
{
  imports = [
    "${nixpkgs-akkoma}/nixos/modules/services/web-apps/akkoma.nix"
    "${nixpkgs-foobarspam}/nixos/modules/path/to/foobarspam.nix"
  ];

  nixpkgs.overlays = [ overlay ];

  services.akkoma.enable = true;

  services.foobarspam.enable = true;
}
```

## Putting it all together

Your flake might end up looking something like this:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-akkoma.url = "github:illdefined/nixpkgs/akkoma";
  };

  outputs = { self, nixpkgs-unstable, nixpkgs-akkoma }: {
    overlay = final: prev: {
      inherit (nixpkgs-akkoma.legacyPackages.${prev.system})
        akkoma akkoma-frontends;
    };

    nixosModule = { pkgs, ... }: {
      imports = [
        "${nixpkgs-akkoma}/nixos/modules/services/web-apps/akkoma.nix"
      ];

      nixpkgs.overlays = [ self.overlay ];

      # An option that the PR added
      services.akkoma.enable = true;
    };
  };
}
```

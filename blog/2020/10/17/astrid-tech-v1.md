---
title: astrid.tech v1.0!
date: 2020-10-17
description: A reflection on the project and on imposter syndrome
tags:
  - /projects/astrid-tech/
  - notes
  - tips-and-tricks
  - computer-science
  - impostor-syndrome
  - /projects/quest-of-con/
  - /projects/tanksberry-pi/
  - /projects/kiwibot/
  - /projects/hascas/
  - haskell
  - rust
---

astrid.tech v1.0 is finally published after 6 months of active development! Major version 1 has the core functionality of the site, and it's in a state that I would call "complete." It's been a long and tough journey, but now that it's over, I can finally say that I've completed something I've been wanting to do for quite some time now.

## Stuff I learned along the way

I learned **so much** while developing this project. Not just how to use entire frameworks and languages like React, GraphQL, CSS, and Gatsby.js, but also some smaller stuff, including but not limited to:

- Setting up continuous integration/deployment makes your life extremely easy, and it only takes a few hours to do
- There's ways to make the site pretty without using much JS
- Drawing out state machines for complex behavior makes your life so much easier
- Jesus Christ, I did [so many random projects](/projects/) back in my high school days
- Jesus Christ, I actually finished more projects than I remember finishing
- Circular images are just normal images but with `border-radius` values equal to their `width` and `height`
- Sometimes, you really don't need any fancy algorithmic crap; all you need is a simple equation. For example, I used a variation of the parallel resistor formula $\left(\frac{1}{\frac{1}{a} + \frac{1}{b} + \frac{1}{c} + \dots}\right)$ to check how similar two projects are.
- TypeScript's type system is _amazing._ It's got unions and intersections, and its type inference system can even do this kind of stuff:

```typescript
type MyType = { foo: "a" } | { foo: "b"; beta: string }

const x: MyType = ...

if (x.foo == "a") {
  // TS detects that this won't work because beta is
  // not on MyType's where foo = "a"
  doSomething(x.beta)
}
```

## Impostor Syndrome

Building a personal website gave me an opportunity to do a little bit of self-appreciation. If there's anything I've learned about CS in the past 8 years, it's that this field is like a hydra. Every time I learn something new, I learn about three more related things. Every step I go up, the ceiling goes up farther, and it feels like I'm constantly falling behind. I think this phenomenon is why I, like many other engineers, I suffer from impostor syndrome. I sometimes wonder if I'm really as skilled as others say I am, or if I'm really out of my league compared to everyone else.

But the process of making this website has given me ammo to use against my inner critic. I spent a good amount of time scrolling through my GitHub repos to look at old projects to add to the page. I also looked at my old Bitbucket account, which I hadn't used in years (it still had my deadname on it, even), to look for private repos I could "declassify."[^1]

I found that a lot of those ancient projects I did were surprisingly advanced. There were even feats of me being too ignorant to realize that what I was doing had a name, and I was [convergently evolving](https://en.wikipedia.org/wiki/Convergent_evolution) the wheel.

- [Quest of Con](/projects/quest-of-con), which I made in my third year of high school, used bilinear interpolation in the terrain generation pipeline. I didn't even know the term at the time, I just invented the method and used it! It came up when I took CSC 357 at Cal Poly, which is supposed to be a second or third year (_of college_) course.
- A lot of the games I made back when I was 14 or so used the [factory pattern](https://en.wikipedia.org/wiki/Factory_method_pattern) to spawn enemies. Except, I didn't even know it was called the factory pattern at the time, I just made a `class Foo extends Enemy` and a `class FooCreator implements CreatorInterface` with a `createEnemy` method.
- I would have forgotten about the ridiculous robots I made, like [Tanksberry Pi](/projects/tanksberry-pi/) and [KiwiBot](/projects/kiwibot/), if I had not spent the time reflecting on my work.

This isn't me trying to say that I've completely mastered CS. Far from it, my career is only just _starting_. I'm only a second-year student, not even a full-time engineer yet! But what I've learned from this experience is that even though you'll never run out of things to learn, you should never for a moment believe that you're an impostor. You deserve to be where you are because you are there. You couldn't have gotten where you are without all the hard work you've put into it. Never stop improving yourself!

## What's next for me?

In the spirit of always improving oneself, I need a new project to waste my spare time on! I've thought up of some potential ideas.

- I still have ideas for various improvements to this website collecting dust on my [Kanban board](https://github.com/astralbijection/astrid.tech/projects/1). However, I kinda want to take a bit of a break from developing this website and do some other stuff first.
- Now that I'm quarantined at home rather than at school, I have a 3D printer and electronics components at my disposal. I kinda want to do that stuff again...
- I've really been wanting learn Haskell and Rust.
  - I had suspended the [HasCAS](/projects/hascas/) project to get this website done, so I might pick that up again.
  - My computer has been running the [i3 window manager](https://i3wm.org/) for a while, and I've been wondering it's time to change it up. Perhaps I could dip my toes into [XMonad](https://xmonad.org/)?
  - I don't know what I could write in Rust, but there's definitely something.
- The last Android app I worked on was a [FRC scouting app](/projects/panther-scouting-app) that I scrapped. Some Android stuff might be interesting to do.
- ~~A IoT project with a Haskell server and Rust firmware that interacts with a cross-platform mobile app~~

Feel free to comment if you have any suggestions for me!

[^1]: _BaCk in mY DaY_, GitHub didn't offer private repos, and I didn't know about GitLab either. That's why they were on Bitbucket.

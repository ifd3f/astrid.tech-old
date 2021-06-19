---
title: A third redesign?
date: 2021-05-03 10:17:05
description: The grass is always greener on the other side
tags:
  - /projects/astrid-tech
  - website
  - rust
  - scala
  - haskell
  - python
  - django
---

So we're already on astrid.tech v2, despite the fact that the footer _still_ says v1. However, I'm feeling like my website is not very flexible anymore, and I want to redesign.

Why the hell do I want to redesign my website again? There are a couple reasons.

1. **My current setup is inflexible, hacked-together, and shitty.** It would be quite a pain to add a new post type, like a note, recipe, or a RSVP, or other things that [IndieWeb people post](https://indieweb.org/posts#Types_of_Posts).

2. **My website is somewhat bloated.** JavaScript makes up a whopping 300 KB of stuff downloaded on the homepage! I want to cut down on that value.

3. **I want to connect my website with other services.** For instance, I want to [syndicate my posts to Twitter, Mastodon, or other services](https://indieweb.org/POSSE), and [syndicate other data back onto my website](https://indieweb.org/PESOS) automatically, and to do that would require a bunch of additional code. I also want to send and receive webmentions, and in a less-janky way than I have now.

The only question now is, what am I going to use to redesign my website? I have a couple of criteria in considering this.

- I like having my posts and content being markdown files in a Git repository. It makes everything editor-agnostic and [avoids pitfalls with database storage of posts](https://indieweb.org/database-antipattern).
- I like statically-typed languages over dynamically-typed languages.
- I'm interested in trying something new. Maybe a wacky functional or functional-ish language, like Scala, which I'm familiar with, or Haskell or Rust, which I've never used before.
- Keeping my 8000 LOC of React would be great, but I may just have to end up throwing it out. Oh well.

There are a couple of decisions I can make.

- **Go fully dynamic.** This would make programming complicated stuff a bit easier, but it would leave my website possibly prone to attacks. I have a couple of options for doing this.
  - _Extend my Python/Django API server._ This would be an easy option, but I honestly don't like Python all that much because it's a dynamically-typed language.
  - _Extend my Rust/Rocket link shortener._ Rust is also pretty cool, though I'm not too familiar with it. I could just build off of my link shortener, though.
  - _Fuck it, brand new server!_ I could write a new server in Scala. I heard they also improved the compile times since I last used it 6 years ago. It also has pretty good tooling.
- **Stay with the strange static/dynamic thing.** To receive webmentions and webhooks and comments, after all, I _will_ need a running server.
  - _Go back to Gatsby._ It's an appealing option; Gatsby includes more batteries for blog sites than Next.js. But then again, I do want to try something new.
  - _Write my own static site generator!_ This has been in the works for a while. I'm experimenting with writing one in Haskell right now. Unfortunately, I'm shit at Haskell, so it's going about as well as you'd expect.

I think it's still possible to preserve some of my React libraries and UI, though. I would just serve it as [custom elements](https://reactjs.org/docs/web-components.html). 

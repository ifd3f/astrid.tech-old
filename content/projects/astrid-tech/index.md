---
type: project

title: astrid.tech v2
status: complete
featured: true
description: This very website
startDate: 2021-12-30
endDate: null
tags:
  - react-js
  - typescript
  - next-js
  - django
  - python
  - docker
  - ci-cd
  - html
  - javascript
  - css
  - sass
  - bootstrap-css
  - website
  - under-construction
  - gatsby-js
url: https://astrid.tech
source:
  - https://github.com/astralbijection/astrid.tech
thumbnail: ./thumbnail.png
---

After having made a lot of personal projects, I realized that I had forgotten a
ton of them, and there wasn't really a place to show them off. This website was
made to solve that issue.

<iframe
  src="https://astrid.tech"
  title="My website, but with recursion"
  width="400"
  height="250">
</iframe>

## Feature List

- Tagging system
  - Relatedness measures between tagged items
- List of projects
- Blog, translated from Markdown
- Comments on blog and project pages
- Pie chart of open source licenses
- A resume

## Design Details

### Tech stack summary

- **Frontend** - statically generated, optimized SPA
  - **Technologies**
    - [Next.js](https://nextjs.org/)
    - [TypeScript](https://www.typescriptlang.org/)
    - [React.js](https://reactjs.org/)
    - [Sass](https://sass-lang.com/)
    - [Bootstrap 4](https://getbootstrap.com/)
  - **Deployment**
    - Github Actions
    - Github Pages
- **Backend** - API-only backend (except for the admin panel)
  - **Technologies**
    - [Python](https://www.python.org/)
    - [Django](https://www.djangoproject.com/)
    - [Django Rest Framework](https://www.django-rest-framework.org/)
    - [Postgres](https://www.postgresql.org/)
  - **Deployment**
    - [Docker](https://www.docker.com/)
    - [Docker Hub](https://hub.docker.com/) (with automated testing)
    - Docker Compose

### Frontend

I use Next.js to statically generate the website.

The static site is hosted via
[Github Pages](https://github.com/astralbijection/astrid.tech) to statically
host the website.

There is a
[Github Actions](https://github.com/astralbijection/astrid.tech/actions)
workflow that automatically builds on every push to verify that my code
compiles. Additionally, if there was a push to the `main` branch, it will
publish the build output to the
[astralbijection.github.io](https://github.com/astralbijection/astralbijection.github.io)
repo.

#### UX design methodology

I've tried to make the browsing experience as user-friendly and inclusive as
possible by:

- Adopting a mobile-first methodology for designing views, and responsively
  sizing elements in CSS
- Statically generating the site to reduce bandwidth consumption for users
- Utilizing semantic tags and designing the site to be accessible for screen
  readers

### Backend

Currently, the backend's only functionality is to serve and receive possibly
anonymous comments.

I have pointed
[Docker Hub](https://hub.docker.com/repository/docker/astridyu/astrid_tech_api)
at my repo to build, test, and release a new `:latest` image on every push to
main. It is manually deployed as a Docker container on a
[free Oracle Cloud Infrastructure](https://www.oracle.com/cloud/free/) VPS.

### Content

The content is in its own
[Git submodule](https://github.com/astralbijection/astrid.tech-content) as a set
of raw files that get aggregated and compiled on each frontend build. It is
written in the following file formats:

- Markdown
- YAML

There used to be (relatively buggy) Jupyter notebook support for a few blog
posts during v1, but that has been suspended. Instead, I used
[Pandoc](https://pandoc.org/) to convert those notebooks into Markdown and raw
images.

## History

### Development Hell

I had always wanted to build my own website.
[I had a friend in 5th grade who had his own website, made in Google Sites](http://billy.lawson.me/)
(though he clearly hasn't updated it since then), and I wanted to be like him.
So, there were multiple attempts over the years with Google Sites websites but
they never worked. I also tried making personal websites on
[Weebly](https://weebly.com) and [Wordpress.com](https://wordpress.com), but I
made the site... and realized that I actually didn't have anything to publish

Fast-forward to 2017, I had gotten several interesting projects under my belt, I
was a seasoned Python and Java dev. I decided, "I'm gonna make my own website!
I'm gonna finally do it!" I bought the domain names to _force_ myself to do it,
otherwise I'd be wasting money, and if you know me, I hate wasting money. And
guess what happened? I didn't do it. Too many other projects, plus I didn't
really like frontend development. I sat on the domain names for a few years,
before I ended up just tossing them out because I no longer used those
usernames.

### Initial create-react-app design

In January 2020, I was taking [CPE 133](/projects/basys3-1d-cellular-automata),
and I met [Monty Choy](https://montychoy.com/). He showed me his website, made
using React and hosted on Github Pages, and I thought it was super cool! I
wanted to make one myself. However, I was super busy with work and everything,
so I put it off for a while.

Then, it was late March 2020, spring break. The pandemic had just started, and I
was depressed because all my friends had left for home until I was the only one
remaining. So I thought, why not finally learn React and build that website? As
much as I wanted to, I didn't get it done during spring break, and spring
quarter was really busy because of the online learning plus the job I had.

At the very end of spring quarter, I realized there were a set of deadly flaws
with create-react-app that would severely impact my website's performance:

- It was not statically generated. You _need_ JavaScript to use it.
- If a user wanted to read my Markdown posts, what would happen is:
  1. On connecting to the site, they download every single markdown post file
  2. On reading the post, their browser takes the markdown post, parses it, and
     converts it to HTML
  3. Finally, the HTML gets embedded inside React. This is very poor in terms of
     perforamnce.

I had a very short foray into React-Static to hopefully fix this problen, but
that never panned out. Instead, I wanted to try something different.

### v0 and v1 - Gatsby Site

I broke ground on this website during the summer of 2020. I took
[the Gatsby Starter Blog template](https://github.com/gatsbyjs/gatsby-starter-blog.git)
template and heavily modified it to suit my needs until it was no longer
recognizable from the original.

- v0.1.0 was the first published version of the site. It just had a blog, an
  about page, and not too much else.
- v1.0.0 was the first version of the site that had most of the features you see
  today - resume and portfolio.

I released v1 at the end of the summer, after days of full-time internship work
followed by nighttime website work.

I had a few blog posts written as Jupyter notebooks, and wrote a plugin that
transformed those notebooks to Markdown.

#### CI/CD Pipeline

There was a
[Github Actions](https://github.com/astralbijection/astrid.tech/actions)
workflow that automatically built my website on every push to verify that my
code compiles. If the push went to the `main` branch, it would additionally
publish the build output to the
[astralbijection.github.io](https://github.com/astralbijection/astralbijection.github.io)
repo, so that Github Pages could statically host it.
[astralbijection.github.io](https://astralbijection.github.io) used to take you
to [astrid.tech](https://astrid.tech) in this way! However, now it does not.

### Personal Branding

I met [Alex Solis](https://spunkyalexsolis.com/) in the
[Out in Tech](https://outintech.com/) Slack. He's a frontend software engineer
with a lot of knowledge about branding, helped me set up my own branding
guidelines. That's how I decided on my dark blue primary color and orange
accents, and pushed my newly rebranded website by the end of December.

### The Commenting Backend

I discovered the [IndieWeb](https://indieweb.org) at some point. I don't
remember how I discovered it, but I joined some Homebrew Website Club meetings
and met others who also run their own websites. Some discussion at a
[Homebrew Website Club meeting](https://indieweb.org/Homebrew_Website_Club) led
me to think about how I want people to interact with my website. So, in hopes
that people would comment on my site, I wrote a backend in Django whose sole
purpose is to host comments. Those

### v2 - Next.js

In January,
[I decided to move it to Next.js](https://astrid.tech/2020/12/20/0/backend#replacing-gatsby)
for a couple of reasons:

- I might want to have SSR rather than SSG at some point, to make it more
  flexible.
- I wanted to try a new frontend framework.
- The company behind Gatsby seems to have done some less-than-ethical things.

So, I spent a chunk of time porting everything to Next.js.

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
url: https://astrid.tech
source:
  - https://github.com/astralbijection/astrid.tech
thumbnail: ./thumbnail.png
---

After having made a lot of personal projects, I realized that I had forgotten a
ton of them, and there wasn't really a place to show them off. This website was
made to solve that issue.

This website used to be written in Gatsby, but I decided to
[move it to Next.js](https://astrid.tech/2020/12/20/backend#replacing-gatsby)

<iframe src="https://astrid.tech" title="My website, but with recursion" width="300" height="250"></iframe>

## Feature List

- Tagging system
  - Relatedness measures between tagged items
- List of projects
- Blog
- Comments on blog and project pages
- Pie chart of open source licenses
- A resume

## Tech stack summary

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

## Frontend

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

### UX design methodology

I've tried to make the browsing experience as user-friendly and inclusive as
possible by:

- Adopting a mobile-first methodology for designing views, and responsively
  sizing elements in CSS
- Statically generating the site to reduce bandwidth consumption for users
- Utilizing semantic tags and designing the site to be accessible for screen
  readers

## Backend

Currently, the backend's only functionality is to serve and receive possibly
anonymous comments.

I have pointed
[Docker Hub](https://hub.docker.com/repository/docker/astridyu/astrid_tech_api)
at my repo to build, test, and release a new `:latest` image on every push to
main. It is manually deployed as a Docker container on a
[free Oracle Cloud Infrastructure](https://www.oracle.com/cloud/free/) VPS.

## Content

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

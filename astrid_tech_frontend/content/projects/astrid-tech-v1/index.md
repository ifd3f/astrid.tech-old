---
type: project

title: astrid.tech v1
status: complete
featured: true
description: An older iteration of this very website
startDate: 2020-03-01
endDate: 2021-01-11
tags:
  - react-js
  - typescript
  - gatsby-js
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
keywords:
  - Gatsby.js
  - React.js
  - TypeScript
  - Sass
url: https://astrid.tech
source:
  - https://github.com/astralbijection/astrid.tech
thumbnail: ./thumbnail.png
highlights:
  - Utilized Gatsby to statically generate the website from raw YAML and
    Markdown files.
  - Designed user-friendly interactive views in React.
  - Applied responsive web design concepts to ensure the website fully takes
    advantage of mobile, tablet, and desktop.
  - Configured continuous integration using GitHub Actions to build, deploy, and
    publish the website.
---

**This document describes an older version of this website. This document is
kept here as a historical reference.**

After building [v1](https://astrid.tech/projects/astrid-tech-v1), I wanted to
rewrite the whole thing in Next.js.

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
    - [Gatsby.js](https://www.gatsbyjs.com/)
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
    - [Let's Encrypt SSL](https://letsencrypt.org/)
    - Docker Compose

## Frontend

I used Gatsby to statically generate the website. However, for
[v2](https://astrid.tech/projects/astrid-tech), I decided to
[move to Next.js](https://astrid.tech/2020/12/20/backend#replacing-gatsby).

The static site was hosted via
[Github Pages](https://github.com/astralbijection/astrid.tech) to statically
host the website. (Going to
[astralbijection.github.io](https://astralbijection.github.io) takes you here!)

There was a
[Github Actions](https://github.com/astralbijection/astrid.tech/actions)
workflow that automatically builds on every push to verify that my code
compiles. Additionally, if there was a push to the `main` branch, it will
publish the build output to the
[astralbijection.github.io](https://github.com/astralbijection/astralbijection.github.io)
repo.

### UX design methodology

I tried to make the browsing experience as user-friendly and inclusive as
possible by:

- Adopting a mobile-first methodology for designing views, and responsively
  sizing elements in CSS
- Statically generating the site to reduce bandwidth consumption for users
- Utilizing semantic tags and designing the site to be accessible for screen
  readers

## Backend

The backend's only functionality was to serve and receive possibly anonymous
comments.

I have pointed
[Docker Hub](https://hub.docker.com/repository/docker/astridyu/astrid_tech_api)
at my repo to build, test, and release a new `:latest` image on every push to
main.

It was manually deployed as a Docker container on a
[Contabo](https://contabo.com) VPS located in Missouri for about \$8/mo. See
[the `docker-compose.yml`](https://github.com/astralbijection/astrid.tech/blob/main/docker-compose.yml)
for deployment details.

## Content

The content was in its own
[Git submodule](https://github.com/astralbijection/astrid.tech-content) as a set
of raw files that get aggregated and compiled on each frontend build. It is
written in the following file formats:

- Markdown
- YAML
- Jupyter notebooks for a few blog posts (I wrote a custom Gatsby plugin that
  auto-transforms the Jupyter notebooks into Markdown files)

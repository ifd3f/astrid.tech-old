---
type: project

title: astrid.tech
status: complete
featured: true
description: This very website
startDate: 2020-03-01
endDate: null
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
  - https://github.com/Plenglin/astrid.tech
thumbnail: ./thumbnail.png
highlights:
  - Utilized Gatsby to statically generate the website from raw YAML and Markdown files.
  - Designed user-friendly interactive views in React.
  - Applied responsive web design concepts to ensure the website fully takes advantage of mobile, tablet, and desktop.
  - Configured continuous integration using GitHub Actions to build, deploy, and publish the website.
---

After having made a lot of personal projects, I realized that I had forgotten a ton of them, and there wasn't really a place to show them off. This website was made to solve that issue.

<iframe src="https://astrid.tech" title="My website, but with recursion" width="300" height="250"></iframe>

## Feature List

- Tagging system
  - Relatedness measures between tagged items
- List of projects
- Blog
- Comments on blog and project pages
- Pie chart of open source licenses
- A resume

### Planned

- Webmentions, sending and receiving
- Feed aggregation of activity from all my accounts, plus my self-hosted "microblogging" system

## Tech stack summary

- **Frontend** - statically generated, optimized SPA
  - **Technologies**
    - Gatsby.js
    - TypeScript
    - React
    - Sass
    - Bootstrap 4
  - **Deployment**
    - Github Actions
    - Github Pages
- **Backend** - API-only backend (except for the admin panel)
  - **Technologies**
    - Python
    - Django
    - Postgres
  - **Deployment**
    - Docker
    - Docker Hub (with automated testing)
    - Docker Compose

## Frontend

I use Gatsby to statically generate the website. However, [I am considering moving off of it to Next.js](https://astrid.tech/blog/2020-12-20-adding-a-backend/#replacing-gatsby). 

The static site is hosted via [Github Pages](https://github.com/plenglin/astrid.tech) to statically host the website. (Going to [plenglin.github.io](https://plenglin.github.io) takes you here!)

There is a [Github Actions](https://github.com/plenglin/astrid.tech/actions) workflow that automatically builds on every push to verify that my code compiles. Additionally, if there was a push to the `main` branch, it will publish the build output to the [plenglin.github.io](https://github.com/Plenglin/plenglin.github.io) repo.

### UX design methodology

I've tried to make the browsing experience as user-friendly and inclusive as possible by:

- Adopting a mobile-first methodology for designing views, and responsively sizing elements in CSS
- Statically generating the site to reduce bandwidth consumption for users
- Utilizing semantic tags and designing the site to be accessible for screen readers

## Backend

Currently, the backend's only functionality is to serve and receive possibly anonymous comments.

I have pointed [Docker Hub](https://hub.docker.com/repository/docker/astridyu/astrid_tech_api) at my repo to build, test, and release a new `:latest` image on every push to main. 

It is deployed as a Docker container on a [Contabo](https://contabo.com) VPS located in Missouri for about \$8/mo. See [the `docker-compose.yml`](https://github.com/Plenglin/astrid.tech/blob/main/docker-compose.yml) for deployment details. I will soon set up [Watchtower](https://github.com/containrrr/watchtower) to automatically pull the image, so that the only thing I need to do with deployment is push to `main`.

## Content

The content is in its own [Git submodule](https://github.com/Plenglin/astrid.tech-content) as a set of raw files that get aggregated and compiled on each frontend build. It is written in the following file formats:
  - Markdown
  - YAML
  - Jupyter notebooks for a few blog posts (I wrote a custom Gatsby plugin that auto-transforms the Jupyter notebooks into Markdown files)

---
type: project

title: astrid.tech
status: complete
featured: true
description: A statically generated portfolio website built on React and Gatsby
startDate: 2020-03-01
endDate: 2020-10-08
tags:
  - react-js
  - javascript
  - typescript
  - gatsby-js
  - html
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

<iframe src="https://astrid.tech" title="My website, but with recursion" width="300" height="250"></iframe>

I made this site to showcase all the stuff I've made. I've tried to make the browsing experience as user-friendly and inclusive as possible by:

- Adopting a mobile-first methodology for designing views, and responsively adjusting elements in CSS
- Statically generating the site using Gatsby to reduce bandwidth consumption for mobile users
- Utilizing semantic tags and designing the site to be accessible for screen readers

## Technology Stack

### The Code

- "Backend"
  - Language: TypeScript
  - Statically generated using [Gatsby](https://www.gatsbyjs.org/)
- Frontend
  - Language: TypeScript
  - Sass
  - React
  - Bootstrap 4

### Languages the content is written in

- Markdown
- Jupyter notebooks for a few blog posts
  - **I wrote a custom Gatsby plugin** that transforms the Jupyter notebooks into Markdown files, which theemselves get transformed into blog posts.

### Deployment

- [GitHub Pages](https://github.com/plenglin/astrid.tech) to statically host the website. (Going to [plenglin.github.io](https://plenglin.github.io) takes you here!)
- [GitHub Actions](https://github.com/plenglin/astrid.tech/actions) to automatically build and publish the site on a push to the main branch.

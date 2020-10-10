---
type: project

title: Collision Zone
status: complete
featured: true
description: IO game where you crash trucks into each other and do wacky jukes
startDate: 2019-06-01
endDate: 2019-08-30
tags:
  - javascript
  - typescript
  - html
  - css
  - bootstrap-css
  - website
  - websockets
  - cpp
  - node-js
  - aws
url: https://collision.zone
source: []
thumbnail: ./thumbnail.gif
highlights:
  - Built an efficient, single-instance game server in C++ and a matchmaking server in Node.js to ensure the app is scalable.
  - Designed a custom binary client-server communication protocol to efficiently minimize WebSocket bandwidth usage.
  - Deployed the server on AWS EC2.
  - Analyzed playtester feedback to tune game parameters and improve player experience.
---

![The hectic game with multiple AI players.](./thumbnail.gif)

An IO-style game that involves cars crashing into each other.

The initial prototype was created during [HSHacks III](https://github.com/Plenglin/HSHacks-III) back in 2017 under the name of "High Octane Elastic Snowploughs." There were many incomplete rewrites of the project until eventually I completed this project in 2019.

---
title: MD-SAPT
status: wip
description:
  A software package for analyzing molecular dynamics trajectories using quantum
  mechanics methods
startDate: 2021-01-07
endDate: null
thumbnail: null
url: https://mdsapt.readthedocs.io
source:
  - https://github.com/calpolyccg/MDSAPT
tags:
  - python
  - ci-cd
  - chemistry
  - cal-poly
---

MD-SAPT is a Python package for analyzing molecular dynamics (MD) trajectories
using a quantum method called symmetry-adapted perturbation theory (SAPT).

We presented it at the American Chemical Society (ACS) Spring 2022 conference in
San Diego, at the Sci-Mix and Computational Chemistry poster sessions. At the
Computational Chemistry session, we were one of only 7 posters that got to the
second round of judging.
[Here's my post detailing my trip.](/2022/03/24/0/acs-spring-2022)

If you're wondering why I'm working on this random chemistry project despite
being a CS major, that's because my chemistry major girlfriend
[Alia Lescoulie](https://alescoulie.github.io/) needed help setting up the CI/CD
pipeline, packaging, and package/documentation deployment, so I helped her out
and kinda got dragged into it.

## My contributions

- Providing my `gfdesk` server as a development machine
- Harmonizing the conda development environment so that it works on MacOS _and_
  Linux
- Fixing the CI/CD pipeline
- Setting up the Anaconda package build recipe so that all you have to do to
  install it is run `conda install -c psi4/label/dev -c conda-forge mdsapt`
- Setting up the readthedocs.io deployment
- Various code optimizations, such as reducing readthedocs.io deployment times
  form 500s to 100s

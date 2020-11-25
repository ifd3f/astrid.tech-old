---
title: Planet-Asteroid Collision
status: complete
description: A GPU-accelerated simulation of a planet hitting an asteroid
startDate: 2020-10-10
endDate: 2020-11-25
tags:
  - opengl
  - glsl
  - cpp
  - simulation
  - /education/cal-poly/csc-572/
url: null
source:
  - https://github.com/Plenglin/572-Planet-Collisions/
---

## Introduction

Our project is a simulation of an asteroid impacting the Earth. We were inspired by [this GitHub repo](https://github.com/mikkel92/Planet-asteroid-interaction).

## Results

https://youtu.be/4lHT7ixTdS0

After the first collision, there are 552 particles being simulated at:

- 60 FPS running on Ubuntu 20.04 (i5-8300H, GTX 1060M)
- 20 FPS running on Windows 10 (i7-7820, GTX 1070)

Without GPU-accelerated computation, these would be running at 20 FPS and 1.5 FPS respectively.

## How it works

### Fragmentation

At the beginning, there are two particles, an **asteroid** and a **planet**, on a collision course with each other. When they collide, they are deleted and replaced with 64 chunks and 488 chunks respectively.

Our chunks were made in Blender by slicing meshes and filling them in.

![Blender](https://i.imgur.com/OtPyYSc.png)

### Physics Engine

Our physics engine runs in the following 4 steps:

#### 1. GPU Phase

The GPU runs through the cartesian product of all particles, and calculates both gravitational force and intersections. Gravitational force is summed up per-particle, and intersections are recorded in a array list.

When the CPU receives the GPU's output, the gravity is written to the particles and the intersecting ones are recorded in a contact index.

#### 2. Intersection Bookkeeping

Contacts keep track of an internal state to improve the stability of low-relative velocity contacts. This phase performs the following tasks:

    Creates new contacts that did not previously exist
    Updates old contacts that still exist
    Deletes old contacts that no longer exist

#### 3. Contact Solving

I describe this step in detail in [this blog post](https://astrid.tech/blog/2020-11-22-n-body-collision/).

#### 4. Integration

This phase performs a single Euler step of position and rotation.

Position is integrated with

$$s_{k+1} = v \cdot \Delta t + s_k$$

where \(s\) is position, \(v\) is velocity, and \(\Delta t\) is the time step.

Rotation is stored in a matrix \(\Theta\) to avoid gimbal lock. Angular velocity is a vector \(\omega\) which is in the direction that the axis the object is rotating around using the right-hand rule, scaled to the speed in radians per second.

Suppose \(M\) is the matrix representing a rotation of \(|\omega|\cdot \Delta t\) around \(\omega\). Thus, we integrate rotation with

$$\Theta_{k+1} = M \cdot \Theta_k$$

---
title: Iterative n-body Collision Problem
date: 2020-11-22
description: An essential part of a complete rigid-body physics engine
tags:
  - notes
  - math
  - physics
  - /education/cal-poly/csc-572/
  - /projects/planet-collision-572/
---

For my CSC 572 final project, I implemented a 3D rigid-body physics engine. Naturally, the problem of n-body collisions arises and we need to figure out what to do about them. Here are notes I wrote about my implementation.

## Structs

Particles are spheres. They essentially contain the following information:

- mass $$m$$
- radius $$r$$
- moment of inertia
- position vector $$s$$
- velocity vector $$v$$
- rotation matrix $$R$$
- angular velocity vector $$\omega$$ (right-hand rule, scaled to angular velocity in radians)

Contacts essentially contain the following information:

- a reference to the two particles, A and B. A and B both contain references to the contact, as well.
- the position where it happened $$P$$
- the **contact normal** $$n$$, which is a vector from B to A, with length equal to the penetration depth
- a state value that says if this contact is stable or not

Every loop, we calculate our list of contacts. This, along with gravity calculation, are the most computationally expensive operations simply due to it being $$O(n^2)$$ operations, so they are offloaded onto the GPU.

## Calculating our list of contacts

This is a $$O(n^2)$$ operation that just checks if two particles are intersecting. If they are, it pushes a new contact onto the list `contacts`. We are currently working on parallelizing it on the GPU.

## n-body contact solving

Once we have our list of contacts `contacts`, to take care of the n-body case, we essentially do the following in pseudo code:

```
repeat contacts.size() times:
    for contact in contacts:
        contact.solve_momentum()
```

where `contact.solve_momentum()` takes care of the 2-body case described [here](#2-body-problem).

Solving contacts multiple times takes care of cases like the Newton's Cradle. This may seem inefficient in theory because if there's $$n$$ particles, there might be $$n^2$$ contacts and thus $$n^3$$ calls for `solve_momentum()`. However, in practice, most particles spend most of their time floating around freely so momentum calculations can be safely done on the CPU, and this step ends up only taking <10% of total processing time.

### Example

Suppose you have a bunch of balls in a line like this:

```
Velocity  |  > ....
Balls     |  o oooo
```

where `>`, `<`, and `.` are right, left, and zero respectively. We expect this situation to end up like this:

```
.... >
oooo o
```

So, by running it iteratively, we can propagate the momentum through the chain:

```
Before
   > ....
   o oooo

1.  >....
    ooooo

2.  .>...
    ooooo

3.  ..>..
    ooooo

4.  ...>.
    ooooo

5.  ....>
    ooooo

After
    .... >
    oooo o
```

The number of iterations could be tuned, or calculated in a smarter way (I was thinking, maybe calculate contacts within contiguous groups that are all touching each other to reduce the number of iterations) but it works well enough for our purposes.

## 2-body problem

### Momentum

If two balls are leaving each other, or if they have low relative velocity/the contact is marked as "stable," then there is no momentum or friction to be applied.

The 3D momentum problem gets reduced to a 1D momentum problem along the collision normal $$n$$. So, we use $$v \cdot n$$ as our velocity and plug it into the conservation of momentum equation with restitution derived [here](https://en.wikipedia.org/wiki/Coefficient_of_restitution#Derivation).

### Friction

From the final momenta, we derive the impulse $$I$$. Multiplying $$I$$ by some arbitrary coefficient approximates the normal force $$F_n$$, giving us the magnitude of friction $$|f| = \mu F_n$$ where $$\mu$$ is the coefficient of friction.

Next, we need to calculate the relative surface velocity of the particles. Suppose we were only looking at a single particle. The contact point is essentially undergoing two movements:

- it is moving at $$v$$, the particle's translational velocity
- it is moving along the particle's surface due to rotation, or $$\omega \times (P - s)$$

So, the surface velocity at the contact point of a single particle is $$v + \omega \times (P - s)$$. The relative surface velocity $$\Delta v_{surf}$$ is just the difference between the two particles' surface velocities at the contact point.

However, friction can only happen tangentially to the normal, so the direction of friction is actually in the direction of $$\Delta v_{surf} - proj_n(\Delta v_{surf})$$ (its orthogonal component). Normalize this, and multiply by the magnitude derived earlier, and we have our final friction.

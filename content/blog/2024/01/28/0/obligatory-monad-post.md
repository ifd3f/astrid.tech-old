---
title: The obligatory "What is a monad?" blog post
description:
  Everyone who has written enough Haskell needs to write this post apparently
date: 2024-01-28 12:45:50-08:00
ordinal: 0
tags:
  - functional-programming
  - haskell
---

> "A monad is a monoid in the category of endofunctors."
>
> _-- someone who isn't being particularly helpful_

A monad is a big fancy word that means something really abstract. A monad is a
kind of functor. A functor is also a fancy words that means something abstract.
These words are commonly used by functional programmers because they want to
sound fancy and write abstract code.

This post will attempt to go against that trend and be very concrete.

## What is a functor?

We'll start with functors, because you need to make something a functor before you can make it a monad.

Here is Haskell's definition of what a functor is:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

If you aren't familiar with haskell's syntax, this is what it means. In order for a type `f` to be a functor, it must be generic over 1 type (so stuff like Maps, which need a key and value type, are out). It must also implement a function called `fmap`, which:
- takes in a function turning `a` into `b`
- takes in a functor in `a`
- returns a functor in `b`

Let's think about what `fmap` means in the case of something concrete, like a list.

```
fmap :: (a -> b) -> [a] -> [b]
```

- takes in a function turning `a` into `b`
- takes in a list of `a`'s
- returns a list of `b`'s

Doesn't this sound like the `map` function? In fact, this is exactly what it is. In fact, [it is literally defined
in the Haskell source code as so](https://hackage.haskell.org/package/base-4.19.0.0/docs/src/GHC.Base.html#Functor):

```
instance Functor [] where
    fmap = map
```

So, a functor can be thought of as a container of some results that lets you turn it into a container of some other results.

> What happens if your list is empty?

Nothing happens. Just like in `map`. You just get an empty list, except instead of having 0 `a`'s, now you have 0 `b`'s.

### Other things that are functors

#### Containers

Sets, queues, heaps, and arrays are functors in a similar way -- you can map over every element in the container to make a new container.

#### Maps

> Wait, didn't you say maps aren't functors because they take 2 types instead of 1?

That's true, `Map` is not a functor. However, `Map k` is. That's true, `Map` is not a functor. However, if you already have a `k`, then `Map k` is -- thus, `fmap` is defined over the values of the map, and we completely ignore the `k` type.

#### Maybe

`Maybe` is a functor. In fact, you can define it pretty trivially like so:

```
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing
```

This is how most functor definitions end up looking.

## Applicative functors

Applicative functor is a fancy word for "I have 2 functors and I would like to be able to merge those 2 functors together into a new spicy functor." One of the ways to define it in Haskell is this:

```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
```

We have a function called `pure`, which you can think of as wrapping

---
title: Interviewing for Facebook
date: 2021-04-08 12:48:35-07:00
description: ""
tags:
  - coding-test
  - linux
  - devops
  - cli
  - databases
  - internship
  - job-interview
  - facebook
---

I'm pleased to announce that I'm going to be a Production Engineering intern at
Facebook this upcoming summer! Production engineering is similar to DevOps at
other companies, but with software engineering tasks, too. Although it's been
over a month since my interviews, I hope I can leave a record of the process to
help future applicants while I still remember some fragments of it.

## First Contact

I was very busy during the Fall quarter and didn't start applying to internships
until very late, so I was afraid that I wouldn't be able to get anything for the
summer. Even though I had a significant amount of previous internship experience
on my resume, many companies were scaling back their internship programs due to
Coronavirus, which amplified my concerns.

As such, I cast as wide of a net as possible and applied to many different
companies of many different sizes. I hadn't been expecting much to come out of
my application to Facebook's Software Engineering Internship, but I sent it
anyways.

### A Coding Test

Then, in early January, a recruiter from reached out to me, and sent me a link
to a LeetCode-style test with 4 questions. I don't recall what they were
exactly, but there were 2 easy and 2 medium ones, going by LeetCode's difficulty
rankings. I was able to solve the easy ones quite quickly, but I could only
solve one of the medium ones with the optimal solution. I could only figure out
a suboptimal solution for the last one.

As I was running out of time, I submitted it and dejectedly waited for the
response. A few days later, the recruiter told me that I had moved onto the next
stage!

### A Command Line Test

In the meantime, a different recruiter found my resume on a different website,
and reached out to me about applying to the Production Engineering Internship. I
applied to that, and my task was to complete a 20-question quiz about the Linux
command line.

I don't recall the specifics, but there were a lot of questions about very
specific behaviors of Linux commands, that essentially boiled down to:

- What command do you use to check CPU/Memory/IO/Network/process data? No, `top`
  is not one of your choices.
- What combination of flags do you use to accomplish a certain task with this
  command? No, you may not `--help` or consult the `man` pages.

I'd consider myself familiar enough with the command line to do
medium-complexity tasks in it, yet I did not know the answer to most of these
questions. However, I received word that I had somehow passed this test as well!

## Information Sessions

After the assessments comes the interviews. Software engineering goes through 2
coding interviews, while production engineering goes through a coding interview
and a systems interview. The coding interviews for both paths are basically
identical.

Thankfully, Facebook provided a ton of resources to help us get through the
interviews. There were learning sessions for both types. They even allowed you
to sign up for mock coding interviews!

Personally, I would recommend going to the learning sessions. They tell you
exactly what they expect from the interviews. I also did schedule myself for a
single mock coding interview just for practice. If they offered a mock systems
interview, I would have definitely scheduled myself for that one.

## Coding Interviews

Anything covered in Data Structures would be fair game in the coding interview.
This would include:

- Iteration
- Recursion
- Arrays
- Lists
- Graphs
- Stacks
- Trees
- Heaps
- Maps
- Time complexity: big-O notation, best case, and worst case
- ... and more!

Thankfully, they did not cover any dynamic programming stuff. I'm terrible at
dynamic programming.

Additionally, you are allowed to program in C++, Java, JavaScript, or Python. Of
course, I chose Python.

Some further recommendations:

- The code does not need to be syntactically correct. In fact, the code will not
  be evaluated, only read by the interviewer for correctness. You can actually
  write pseudocode if it gets the point across, too. Towards the end of one
  interview, I was having trouble writing an iterative solution, so gave up and
  I wrote a recursive and very pseudocodey solution. I seem to have passed that
  interview.
- The engineer interviewing you judges you based on your thought process. As
  such, you should be talking through every part you write. I would recommend
  doing some rubber duck debugging before coming to the coding interview.
- All coding interviews are identical. The 2nd one is not meant to be harder
  than the 1st one. It may be harder if you got a harder problem, though, in
  which case I'm sorry for your poor luck.
- You should ask your interviewer clarifying questions about the problem,
  possibly writing out some test cases to make sure that both of you are on the
  same page.
- The interviewer might actually give you some hints sometimes.
- It doesn't matter if you write a recursive or iterative solution. I mostly
  wrote recursive solutions because I find them a lot more compact, as well as
  easier to reason about.

### 1st SWE Interview

I forget what my first question was, but it was pretty easy. The second
question, on the other hand, was to
[serialize and deserialize a binary tree](https://leetcode.com/problems/serialize-and-deserialize-binary-tree/):

> Design an algorithm to serialize and deserialize a binary tree. There is no
> restriction on how your serialization/deserialization algorithm should work.
> You just need to ensure that a binary tree can be serialized to a string and
> this string can be deserialized to the original tree structure.

This is a Leetcode-hard question, but it can be fairly easily solved with
recursion in both the `serialize()` and `deserialize()` methods.

### PE Coding Interview

The question was how to find
[$k$-th largest element in an array](https://leetcode.com/problems/kth-largest-element-in-an-array/),
which is a medium problem.

I first solved the solution by sorting the array, then picking the item at index
$k - 1$, which would be $O(n \log n)$.

Then, the interviewer asked me how I could improve it. I responded by rewriting
it to use a min heap as a sort of running tally of top-$k$ elements, which would
make it $O(n \log k)$.

Then, he asked, "what if you have 50 elements and you want to get the 49th?" to
which I responded by reframing the problem not as the $k$-th largest, but as the
$(n - k)$-th smallest and using a max heap. Next, I made the algorithm decide
which problem to solve based on if $k > \frac{n}{2}$, making the problem
$O(n \log \min(k, n - k))$.

### Mock Interview

The first question given here was the
[word search](https://leetcode.com/problems/word-search/) problem, which is
LeetCode medium:

> Given an `m` x `n` grid of characters board and a string word, return `true`
> if word exists in the grid.
>
> The word can be constructed from letters of sequentially adjacent cells, where
> adjacent cells are horizontally or vertically neighboring. The same letter
> cell may not be used more than once.

They also asked a similar problem, where instead of returning `true`, you return
the number of times it shows up.

### 2nd SWE Interview

I was asked to design a Tic-Tac-Toe game. This question is locked behind
Leetcode Premium, but
[other sites](https://aaronice.gitbook.io/lintcode/data_structure/design-tic-tac-toe)
seem to say it's a medium problem.

> Design a Tic-tac-toe game that is played between two players on a $n \times n$
> grid. You may assume the following rules:
>
> 1. A move is guaranteed to be valid and is placed on an empty block.
> 2. Once a winning condition is reached, no more moves is allowed.
> 3. A player who succeeds in placing n of their marks in a horizontal,
>    vertical, or diagonal row wins the game.

## The Systems Interview

Now, this interview was _hard_. The coding ones may have been a bit difficult,
but at least I knew what I was doing then! Here, I felt like I was being asked
**very** detailed questions about stuff, like they were testing the limits of my
knowledge.

According to previous interviewees and Facebook's production engineering splash
page, the Systems Interview covers, among other things:

- Linux/Unix: how it manages processes and memory and user-space and
  kernel-space and more things
- Databases, of SQL and NoSQL variety
- Networking, like load-balancing and stuff
- Web services

I crammed very hard in the weeks before this interview, and even when I showed
up to the interview, I felt a bit uncomfortable.

### `vmstat`

The first problem was to interpret the output of a call to `vmstat`, which looks
something like this (note: output is from my computer and not what was actually
presented to me):

```
procs  -------------memory---------- ---swap-- -----io---- -system-- ------cpu-----
 r  b   swpd     free   buff   cache   si   so    bi    bo   in   cs us sy id wa st
 0  0      0 21279144 209800 4413888    0    0    36    14  207  500  6  1 92  0  0
 0  0      0 21406776 209800 4285952    0    0     0     0 1406 3826  4  1 95  0  0
 0  0      0 21519508 209808 4173476    0    0     0   524 1283 3609  4  1 95  0  0
 0  0      0 21519760 209816 4173412    0    0     0    56 1197 2708  6  1 93  0  0
 0  0      0 21519760 209816 4173412    0    0     0   548 1260 2882  5  1 94  0  0
```

I didn't know what any of the 2-letter things meant, I use `top`! At first, I
pretended to know, and simply guessed what it all meant. But luckily, it seems
that the interviewer realized I was struggling partway through, and told me that
he wasn't quizzing me on my knowledge of the command, but on what its output
meant. So, I was able to ask him what each thing meant.

Still, that didn't make it much easier. He asked me very detailed questions,
covering topics like:

- Process state, and how operating systems multitask
- Kernel vs. user space, how system calls are made
- Virtual memory, swap memory, when to use it
- What kind of load is it, if there's more bytes written than bytes read?

### Database

Eventually, we moved onto the next question. Suppose we have a database that's
been slow lately, and our queries are taking a long time. We suspect it's the
fault of the disk, but how do we figure that out?

He also asked some related questions, but we pretty much covered the following
topics:

- How to fix it if it's the disk's fault
- How to tell if queries are slow, how to analyze the way queries are made,
  poorly-written joins
- Tradeoffs involved in indexing
- Sharding, replication, and the difference between those two
- In-memory caches like Redis, and tradeoffs you would encounter by using those

## Offer!

Less than a week after the last interview, I got an email saying that I was
offered not one, but _both_ positions! I could only pick one, so I chose
Production Engineering. The reason I chose it, despite having gone through a
harder interview, was due to several reasons. First, since it was harder, I'll
end up learning more things. Second, I was working on my
[homelab](/projects/infrastructure) at the time, and I find it to be an
interesting field of study. Third, I think Facebook would be the best place to
do something like Production Engineering, because they offer services to
billions of users, and it would be a perfect opportunity to learn about systems
at scale that way.

The internship will be remote, whether or not conditions change. Still, I think
it will be a great place for me to gain experience, and I hope to learn a lot of
cool stuff!

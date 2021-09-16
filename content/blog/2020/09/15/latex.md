---
title: The possibility of using LaTeX to take notes for Physics class
date: 2020-09-15
description: When LaTeX is your hammer...
thumbnail: ./svg-in-latex.png
tags:
  - notes
  - note-taking
  - school
  - latex
  - physics
---

I don't often take notes, and this is a poor habit of mine. Sure, I end up
getting A's in those classes anyways, but there's always the possibility of me
_not_ getting an A in a particularly difficult class because I didn't take
notes.

This quarter, I'm taking a physics class, and I had an idea: since there's gonna
be equations and crap, what if I were to take notes in $\LaTeX$?

## Setting up the environment

Like previous $\LaTeX$ projects I've done, I set up Visual Studio Code with the
[excellent $\LaTeX$ Workshop plugin](https://marketplace.visualstudio.com/items?itemName=James-Yu.latex-workshop)
and got to work.

I decided on the following hierarchal structure:

- The whole `\documentclass{book}` - The whole class
- `\part` - Different units in the class
- `\chapter` - Every day, or quiz/test notes
- `\section`, `\subsection`... as needed

Here is the folder structure:

```
week1/
  week1.tex
week2/
  week2.tex
...
.gitignore
main.tex
```

and here is the actually very simple main file:

```latex
\documentclass{book}

\usepackage{...}
...

\title{PHYS 132 Notes}
\author{Astrid Yu}

\begin{document}
\maketitle

\tableofcontents

\part{Waves}

\include{./week1/week1}
\include{./week2/week2}
...

\part{Optics}

\part{Thermodynamics}

\end{document}
```

Each week goes in a single .tex file in its own folder, like so:

```latex
\chapter{Simple Harmonic Motion}

\section{2020-09-14}

\subsection{Syllabus}

...

\subsection{Lecture}

\subsubsection{Basics of Oscillations}

\begin{itemize}
    \item Oscillation = motion that repeats itself, back and forth around equilibrium position. Most important is Simple Harmonic Motion (SHM), sinusoidal
    \item Spring goes from expansion to contraction to expansion, etc. Restoring force occurs when stretched or compressed, goes against spring's displacement
    \item If graph repeats, it's oscillatory motion. If position = sinusoidal, then it's SHM.
\end{itemize}

...

\subsection{Lab Partners}

...

\section{2020-09-16}

...
```

## Wait, doesn't physics have lots of diagrams?

Yes it does, I realized in the middle of class, much to my chagrin. Drawing out
diagrams in $\LaTeX$ sounded like hell, so I just opened up Inkscape and started
doodling a diagram to embed in the $\LaTeX$.

But then, also to my chagrin, it seems that you can't just
`\includegraphics{shitty-drawing.svg}` either! What do?

The solution is to use the `svg` package and add the `-shell-escape` flag to
`latexmk`. Now, including Inkscape is as easy as building the following folder
structure:

```
week1/
  week1.tex
  spring.svg
...
```

and inside `week1.tex`, declare a

```latex
\includesvg{week1/spring.svg}
```

and voila!

![A crudely-drawn free-body diagram of a spring inside of a LaTeX book.](./svg-in-latex.png)

You might notice that the image text exactly matches with the $\LaTeX$ font!
This is because the `svg` package actually treats that text as $\LaTeX$ code.
Here's how it looks in Inkscape:

![The same diagram, but inside Inkscape. There are dollar signs around the words to tell LaTeX to compile them into equations.](./inkscape-dollar.png)

A very very small thing with it is that it generates even more intermediate
files, so you just have to add to your `.gitignore`:

```
*.pdf
*.pdf_tex
```

## Conclusion

With this, we now have ourselves a fully set-up workflow for taking lecture
notes in $\LaTeX$ and Inkscape!

![How the result looks](./notes-joined.png)

I have to admit, since $\LaTeX$ is somewhat verbose, this is a bit of a slow
method of note-taking. You won't be able to get as much down as, say, Markdown.
But at the same time, not many markdown editors have $\LaTeX$ equation support.

In addition, knowing me, who knows if I'll have the motivation to keep this up
for another week? Either way, I still think that this method has some potential.
Plus, you do end up with very beautiful notes at the end.

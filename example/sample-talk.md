---
title: Sample Talk
subtitle: nur ein Beispiel
author: Stefan Wehr
date: 11.8.2020
institute: Hochschule Offenburg
---

## Features

`text-slides` has some very cool features:

- Write your slides in markdown (rendered by pandoc)
- Use mathematical formulaes
- Typeset flowcharts with mermaid
- Draw diagrams with keynote
- Extract and test source code embedded in your slides
- Extend `text-slides` by writing plugins in Haskell

## Mathematical formulas

\begin{align*}
  f(x) &= x^2\\
  g(x) &= \frac{1}{x}\\
  F(x) &= \int^a_b \frac{1}{3}x^3
\end{align*}

## Flowcharts with mermaid

~~~mermaid
sequenceDiagram
    participant Alice
    participant Bob
    Alice->>John: Hello John, how are you?
    loop Healthcheck
        John->>John: Fight against hypochondria
    end
    Note right of John: Rational thoughts <br/>prevail!
    John-->>Alice: Great!
    John->>Bob: How about you?
    Bob-->>John: Jolly good!
~~~

## Diagrams with keynote

~~~keynote(file: "my_presentation.key", slide: 2)

## Source code (1)

The following code is extracted to `build/plugins/python/sample-talk.py`.
Executing the code in the file runs all doctests.

~~~python
def foo(x):
    """Increments its argument by one.

    >>> foo(41)
    42
    """
    return x + 1
~~~

## Source code (2)

The following code is appended to `build/plugins/python/sample-talk.py`

~~~python
print(foo(41))
~~~

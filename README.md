# text-slides

Write your slides with markdown, include images from keynote, write diagrams using
[mermaid](https://mermaid-js.github.io/mermaid/#/) and automatically extract source code
from your presentation.

## Usage

See [sample-talk.md](example/sample-talk.md) for a showcase of all features that text-slides
adds to pandoc. See [here](https://daringfireball.net/projects/markdown/) for the documentation
of the markdown syntax.

To generate a HTML version of your presentation, use

~~~
text-slides example/sample-talk.md
~~~

To
render it as PDF use

~~~
text-slides --output-mode pdf example/sample-talk.md
~~~

## Installation

* Clone [this](https://github.com/skogsbaer/text-slides) repository
* Run `stack install`

## Requirements

* [stack](https://docs.haskellstack.org/en/stable/README/), a build tool for Haskell.
* [pandoc](https://pandoc.org)
* For including images from keynote, you need [python3](https://www.python.org) and
  the [appscript](https://pypi.org/project/appscript/) library.
* For writing diagrams with mermaid, you need [mermaid](https://mermaid-js.github.io/mermaid/#/)

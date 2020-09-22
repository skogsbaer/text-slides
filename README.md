# text-slides

Write your slides with markdown, include images from keynote, write diagrams using
[mermaid](https://mermaid-js.github.io/mermaid/#/) and automatically extract source code
from your presentation. `text-slides` relies on [pandoc](https://pandoc.org) for producing
your slides but adds additional features through various plugins.

## Usage

See [sample-talk.md](example/sample-talk.md) for a showcase of all features that text-slides
adds to pandoc. See [here](https://daringfireball.net/projects/markdown/) for the documentation
of the markdown syntax in general, the [CommonMark](https://commonmark.org) and
[github](https://github.github.com/gfm/) extensions are also supported.

To generate a HTML version of your presentation, use

~~~
text-slides example/sample-talk.md
~~~

To
render it as PDF use

~~~
text-slides --output-mode pdf example/sample-talk.md
~~~

## Plugins

Plugins reuse the syntax of [code fences](https://github.github.com/gfm/#code-fence), where
a code fence starts and ends with `~~~` on a separate line. A plugin call looks like this:

```
~~~PLUGIN_NAME(ARG_LIST)
BODY
~~~
```

Or, for plugins without body, like this:

```
~~~PLUGIN_NAME(ARG_LIST)
```

To be more flexible with respect to syntax highlighting in certain editors, you can write
calls without bodies also like this:

```
~~~PLUGIN_NAME(ARG_LIST) ~~~
```

or

```
~~~PLUGIN_NAME(ARG_LIST)
~~~
```

If the `ARG_LIST` is empty, the paranthesis can be omitted.

The `ARG_LIST` is a comma-separated list of key-value arguments. Each argument has the form
`key: value`, where `value` can be a string like `"some string"`, a number like `42` or a boolean
`false` or `true`.


Here is a list of the supported plugins.

### Code plugins

Syntax highlighting and code extraction for various languages. Currently, the following
languages are supported:

* bash
* c
* cs (C#)
* css
* clojure
* erlang
* fsharp
* html
* haskell
* json
* java
* javascript
* typescript
* ocaml
* objectivec
* python
* rust
* scheme
* xml

If your language of choice is not part of this list, but mentioned in the output of
`pandoc --list-highlight-languages`, it is easy to add support for this language to
text-slides.

All code plugins support the following arguments:

* `file` (optional string): the path where the extracted code is placed. Default:
`YOUR_PRESENTATION.EXT` if your presentation is placed in the file
`YOUR_PRESENTATION.md` and `EXT` is the extension of the language you have choosen. The
path is interpreted relative to `build/plugins/LANGUAGE`.
* `mode` (optional string): one of `"show"`, `"hide"` or `"showOnly"`. The default is `"show"`: the
code is displayed on the slides and extracted. Mode `"hide"` extracts the code but does not
show it on the slides, `"showOnly"` shows the code on the slides but does not extract it.
* `lineNumbers` (optional string): one of `"on"`, `"off"` or `"auto"`. The default is `"auto"`:
line numbers are printed if the code is more than 5 lines long.
* `firstLine` (optional string or int): determines the starting line number. An int simply sets
the number of the first line. The string `"continue"` resumes line numbering where
the last code block left off.
* `place` (optional string): determines where in the output file the code is placed.
The default is "here", which respects to ordering of the code snippets in the markdown file.
"atEnd" place the code snippet at the of the code file, if several snippets are marked with
"atEnd", then their ordering in the markdown file is preservered. Similarly, "atStart" places
the code snippet at the beginning of the code file.

#### Example

```
~~~python(file: "foo.py", mode: "hide")
print("Hello World!")
~~~
```

Writes the code to `build/plugins/python/foo.py` and hides it from the presentation.

See section "More code plugins" for how to configure additional code plugins.

### keynote

Converts a keynote presentation into a set of images and includes one of the images in the
presentation.

Supported arguments:

* `file` (required string): path to the keynote presentation.
* `slide` (required int): number of the slide (numbering starts at 1).

Example:

```
~~~keynote(file: "my_presentation.key", slide: 2)
```

Shows the second slide of the presentation `my_presentation.key`.


### mermaid

Draw diagrams with a textual format, render the diagrams as images and include them
in your presentation. Arguments:

* `width` (optional int): width of the generated image
* `height` (optional int): height of the generated image

Example:

```
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
```

## Installation

* Clone [this](https://github.com/skogsbaer/text-slides) repository
* Run `stack install`

## Requirements

* [stack](https://docs.haskellstack.org/en/stable/README/), a build tool for Haskell.
* [pandoc](https://pandoc.org)
* For including images from keynote, you need [python3](https://www.python.org),
  the [appscript](https://pypi.org/project/appscript/) library, and the
  [convert](https://imagemagick.org) commandline tool from ImageMagick.
* For writing diagrams with mermaid, you need [mermaid](https://mermaid-js.github.io/mermaid/#/):
  `npm install -g @mermaid-js/mermaid-cli`

## Configuration

### Header file for beamer presentations

You can specify a file to be included in the header of the latex file for beamer
presentations, see Pandoc's `--include-in-header` option. The file can be specified
in the following three ways:

- Command line flag `--beamer-header FILE`.
- A file named `beamer-header.tex` in the same directory as the input file.
- Through the file `$HOME/.text-slides/beamer-header.tex`.

### Header file for HTML presentations

You can specify a file to be included in the header of the HTML file for beamer
presentations, see Pandoc's `--include-in-header` option. The file can be specified
in the following three ways:

- Command line flag `--html-header FILE`.
- A file named `html-header.tex` in the same directory as the input file.
- Through the file `$HOME/.text-slides/html-header.tex`.

### Lua filters

You can specify a file defining lua filters for pandoc,
see Pandoc's `--lua-filter` option. The file can be specified
in the following three ways:

- Command line flag `--lua-filter FILE`.
- A file named `pandoc-filter.lua` in the same directory as the input file.
- Through the file `$HOME/.text-slides/pandoc-filter.lua`.

### More code plugins

The file `$HOME/.text-slides/code.json` lists additional code plugins. Here is an example:

```json
{ "languages": [
    { "name": "python-repl",
      "syntaxFile": "syntax/python-repl.xml", // optional, resolved relatively to $HOME/.text-slides
      "extension": ".py",
      "commentStart": "#"
      // "commentEnd" is optional
    }
    # add more language definitions here
  ]
}
```

The syntax file is a
[KDE syntax definition file](https://docs.kde.org/stable5/en/applications/katepart/highlight.html).

### Syntax highlighting themes

Pandoc comes with a couple of builtin themes for syntax highlighting. The default used by
text-slides is the `haddock` theme. Here's how to use a different theme:

* Use the commandline option `--syntax-theme=NAME_OF_THEME|FILE`.
* Place the definition of your theme in `syntax-highlighting.theme`,
  in the same directory as the input file.
* Place the definition of your theme in `$HOME/.text-slides/syntax-highlighting.theme`.

For a theme include with pandoc, you can save to definition to a file with this command:
`pandoc --print-highlight-style NAME_OF_THEME > FILE`

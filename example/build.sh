#!/bin/bash

pandoc --from=markdown --to=slidy --mathjax --standalone \
    --slide-level 2 --highlight-style=pygments \
    --output=build/sample-talk.html sample-talk.md

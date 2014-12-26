# CommonDoc

A framework for representing and manipulating documents as CLOS objects.

# Overview

Parsers for Markdown and other markup languages generally implement their
internal representation of document structure, and code to transform it directly
to one or more output formats. Generally, every parser has a different internal
representation, a different set of supported backends, each in various degrees
of completeness.

What CommonDoc provides is a way to separate parser and emitter by providing a
common framework for representing documents. Parsers and emitters can be written
to convert text to and from various markup langages -- Markdown, Textile, ReST,
etc. -- while keeping a single, agnostic internal representation.

# Usage

# License

Copyright (c) 2014 Fernando Borretti

Licensed under the MIT License.

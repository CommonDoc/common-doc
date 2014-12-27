# CommonDoc

[![Build Status](https://travis-ci.org/CommonDoc/common-doc.svg?branch=master)](https://travis-ci.org/CommonDoc/common-doc)

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
etc. -- while keeping a single, backend-agnostic internal representation.

## Motivation

This library can be considered an implementation of Robert Strandh's suggestion
for the creation of a [library for representing documents][strandh]:

>The purpose of this project is to create a library that defines a set of
>classes and generic functions that allow some client code to create and
>manipulate a document. Contrary to the types of systems cited above, the
>specification of this system is thus in terms of what kind of objects it
>manipulates, and what functions exist to manipulate them.

>A library like this can then be used both by an application that reads some
>markup syntax and produces the document in the form of a graph of class
>instances, and by an interactive application that allows the user to create the
>document by issuing gestures. In fact, it will be possible to create several
>different application with different markups and with a different set of
>possible gestures.

[strandh]: http://metamodular.com/Common-Lisp/document-library.html

## Parsers/Emitters

### [CommonHTML](https://github.com/CommonDoc/common-html)

[![Build Status](https://travis-ci.org/CommonDoc/common-html.svg)](https://travis-ci.org/CommonDoc/common-html)

An HTML5 emitter.

### [ParenML](https://github.com/CommonDoc/parenml)

[![Build Status](https://travis-ci.org/CommonDoc/parenml.svg)](https://travis-ci.org/CommonDoc/parenml)

An S-expression markup language.

# Usage

Most documentation can be found in the docstrings. A sample of usage is:

## Examples

Constructing a document with title "My Title", containing a paragraph with a
single text node:

```lisp
(doc
 (<document>
  (:title "My Document"
   :creator "me"
   :keywords (list "test" "test1"))
  (<paragraph>
   ()
   (<text-node>
    (:text "test")))))
```

# License

Copyright (c) 2014 Fernando Borretti

Licensed under the MIT License.

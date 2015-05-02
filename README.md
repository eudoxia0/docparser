# docparser

Extract documentation from Common Lisp systems. It will be used in the
[Codex][codex] documentation generator.

# Overview

docparser is a library for extracting documentation from Common Lisp systems in
a structured manner. It takes a system name and returns a set of objects
representing packages, variables, functions, classes, etc. and their docstrings.

It's not yet another documentation generator. Rather, it simply provides the
documentation extraction layer on top of which you can build a documentation
generator.

Documentation generators generally implement their own (incomplete) version of
docstring extraction. docparser is meant to extract as much information from a
system as possible, and let documentation generators choose what to keep and
discard. This minimizes duplication of effort and separates documentation
extraction from generation.

docparser has classes to represent every documentable Common Lisp construct:

* Functions
* Macros
* Generic functions
* Methods
* Special variables and constants
* Classes and class slots
* Structures and structure slots
* Type definitions
* Packages

## CFFI Documentation

Additionally, docparser custom subclasses to represent the documentation of
[CFFI][cffi] definitions:

* Foreign functions ([`defcfun`][defcfun]).
* Foreign type definitions ([`defctype`][defctype]).
* Foreign structure definition ([`defcstruct`][defcstruct]).
* Foreign union definitions ([`defcunion`][defcunion]).
* Enumerations ([`defcenum`][defcenum]).
* Bitfields ([`defbitfield`][defbitfield]).

This improves API documentation generation for foreign library wrappers. Note
that, when parsing documentation, docparser catches and ignores foreign library
loading errors, so documentation can be generated even in a machine that can't
properly load the library. This is useful for documenting libraries with complex
external dependencies.

[codex]: https://github.com/CommonDoc/codex
[cffi]: https://github.com/cffi/cffi
[defcfun]: https://common-lisp.net/project/cffi/manual/cffi-manual.html#defcfun
[defctype]: https://common-lisp.net/project/cffi/manual/cffi-manual.html#defctype
[defcstruct]: https://common-lisp.net/project/cffi/manual/cffi-manual.html#defcstruct
[defcunion]: https://common-lisp.net/project/cffi/manual/cffi-manual.html#defcunion
[defcenum]: https://common-lisp.net/project/cffi/manual/cffi-manual.html#defcenum
[defbitfield]: https://common-lisp.net/project/cffi/manual/cffi-manual.html#defbitfield

# Usage

```lisp
(docparser:parse :my-system-name)
```

## Extending

You can extend docparser in two ways: Adding new parsers and new classes. Adding
new classes probably won't be very useful unless you also modify the client of
your extension to use them. Adding new parsers that instantiate existing
documentation classes, however, can be very useful.

For instance, you could have a parser that extracts information from a custom
`defwidget` macro in a GUI framework, and creates an instance of `class-node`
with a modified docstring.

Alternatively, if you're writing a documentation generator specific to this
framework, you could create a subclass of `class-node`, `widget-node`, with
extra slots for the added information.

To define a new parser, use the `define-parser` macro. As an example of use,
this is the definition of the parser for `defun` forms:

```lisp
(define-parser cl:defun (name (&rest args) &rest body)
  (let ((docstring (if (stringp (first body))
                       (first body)
                       nil)))
    (make-instance 'function-node
                   :name (if (listp name)
                             ;; SETF name
                             (symbol-node-from-symbol (second name)
                                                      :setf t)
                             ;; Regular name
                             (symbol-node-from-symbol name))
                   :docstring docstring
                   :lambda-list args))))
```

# API

## Common Lisp Classes

### `symbol-node`

### `documentation-node`

### `operator-node`

### `function-node`

### `macro-node`

### `generic-function-node`

### `method-node`

### `variable-node`

### `slot-node`

### `record-node`

### `struct-node`

### `class-node`

### `type-node`

## CFFI Classes

### `cffi-node`

### `cffi-function`

### `cffi-type`

### `cffi-slot`

### `cffi-struct`

### `cffi-union`

### `cffi-enum`

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.

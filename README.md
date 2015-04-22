# docparser

Extract documentation from Common Lisp systems.

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
discard.

# Usage

# License

Copyright (c) 2015 Fernando Borretti

Licensed under the MIT License.

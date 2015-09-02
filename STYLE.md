# Parallax Style Guide

This is the style guide for code written in Parallax.

# Style Guidelines

## General

* Every file should begin (roughly) with the template shown
  in [Appendix 1][ax-1].

## Whitespace

* Always use spaces for indentation.
* Always use Unix-style line endings. Git enforces this.
* Delete all trailing whitespace before committing.
* Never go over 80 characters of width in a file.
* Try to horizontally align things to achieve visual coherence.
    * However, do not ever indent lines such that one cannot
      just select the whole file in Emacs and reindent.

## Syntax

* Bindings in `let`-style expressions should look like this:

```scheme
(let ([x 5]
      [y 3]
      [z 4])
  scope)
```

* Generally, whenever you have something that semantically
  represents a binding of some kind, you should use the same
  style — square brackets nested inside parentheses — as the
  `let` example above.

## Naming

* The length of a variable/function name should correspond to
  the size of its scope.
* The variables `i`, `j`, and `k` are generally reserved for
  loop counter variables.
* A two-letter variable name that ends in `s` should represent
  a sequence structure, i.e.: `xs` could be a list.
* Classes should be surrounded by angle brackets (`<class>`).
* Global/dynamic variables (parameters) should end in `%`.
* Explicit namespacing is almost always unnecessary, as it can
  easily be accomplished with the Guile module system.

## Documentation

* All functions and variables should be defined with documention.
    * Use the following macros to add docstrings to definitions:
        * `define-macro-with-docs`
        * `define-with-docs`
        * `define-generic-with-docs`
        * `define-class-with-docs`
    * These macros are from [`(scheme documentation)`][docstrings].
    * Docstrings have the following rules:
        * First line is a complete sentence ending in punctuation.
        * Begin with a terse, complete sentence.
            * Use imperative language.
            * For example, prefer "Return" over "Returns",
              "Determine" over "Determines", etc.
            * The arguments should each appear in the docstring in
              the same order as they appear in the argument list.
        * Do not indent lines in a docstring.
        * Texinfo formatting can be used inside docstrings.
            * Refer to [Appendix 2][ax-2] for a primer on Texinfo syntax.
            * More information on Texinfo is available [here][texinfo].

## Tests

* Use the [`(srfi srfi-64)`][srfi-64] module for creating unit tests.
* We should have 100% coverage if possible.

## Performance

* Use `define-inlineable` whenever it makes sense to inline a definition.
    * One particular example is when you want to hide the accessor of an
    object from callers: you can use an inlined function wrapper.
* Always precompile regexes with `make-regexp` or the (yet to be defined)
  `rx` macro from `utility.scm`.
* Try not to mutate variables unless it is absolutely necessary.
* Equality predicates:
    * Use `eq?`    when comparing symbols
    * Use `eqv?`   when comparing numbers and characters
    * Use `equal?` when comparing lists/vectors/etc.

# Appendix

## 1. Standard File Template

```scheme
;;; -*- mode: scheme; geiser-scheme-implementation: guile -*-
;;; File: <FILE-NAME>
;;
;;; License:
;; Copyright © 2013-2015 Stephen Chang    <stchang@racket-lang.org>
;; Copyright ©      2015 Remy Goldschmidt <taktoa@gmail.com>
;;
;; This file is part of Parallax.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;;
;;; Author:     Stephen Chang    <stchang@racket-lang.org>
;;; Maintainer: Remy Goldschmidt <taktoa@gmail.com>
;;
;;; Homepage:   https://github.com/taktoa/parallax
;;
;;; Commentary:
;; <LIBRARY-DESCRIPTION>
;;
;;; Code:

(define-module (parallax <LIBRARY-NAME>)
  #:version    (0 5 0)
  #:use-module (parallax utils)
  ;; ... extra imports etc. go here ...
  #:export     ())
```

## 2. Texinfo Reference

| Texinfo                | Meaning                      | Short Example        |
| ---------------------- |:---------------------------- | -------------------- |
| `@var{VARIABLE}`       | Metasyntactic variable       | `@var{foo}`          |
| `@samp{SHELL-CMD}`     | Used for full shell commands | `@samp{ls -l}`       |
| `@code{CODE}`          | Generic code reference       | `@code{null}`        |
| `@command{COMMAND}`    | A shell command              | `@command{ls}`       |
| `@option{OPTION}`      | A shell command option       | `@option{-l}`        |
| `@env{VARIABLE}`       | An environment variable      | `@env{PATH}`         |
| `@key{KEY}`            | A key code, in Emacs format  | `@key{ENTER}`        |
| `@kbd{KEY-SEQ}`        | A key sequence               | `@kbd{C-@key{TAB}}`  |
| `@file{FILE-NAME}`     | A file name                  | `@file{foo.scm}`     |
| `@dfn{TERM}`           | Introducing a technical term | `@dfn{deleting}`     |
| `@abbr{ABBR, EXP}`     | An abbreviation              |                      |
| `@acronym{ACR, EXP}`   | An acronym                   |                      |
| `@url{URL[, TEXT]}`    | A link to a URL              |                      |
| `@email{ADDR[, TEXT]}` | A link to an email address   |                      |
| `@emph{TEXT}`          | Emphasize text               |                      |
| `@strong{TEXT}`        | Emphasize text even more     |                      |
| `@sc{TEXT}`            | Small caps text              |                      |
| `@example`             | Begin an example block       |                      |
| `@end example`         | End an example block         |                      |

--------------------------------------------------------------------------------

[ax-1]: #standard-file-template
[ax-2]: #texinfo-reference

[texinfo]:    http://www.gnu.org/software/texinfo/manual/texinfo/texinfo.html
[srfi-64]:    http://srfi.schemers.org/srfi-64/srfi-64.html
[docstrings]: http://www.nongnu.org/guile-lib/doc/ref/scheme.documentation


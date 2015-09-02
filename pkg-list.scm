;;; -*- mode: scheme; geiser-scheme-implementation: guile -*-
;;; File: pkg-list.scm
;;
;;; License:
;; Copyright © 2015 Remy Goldschmidt <taktoa@gmail.com>
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
;;; Author:     Remy Goldschmidt <taktoa@gmail.com>
;;; Maintainer: Remy Goldschmidt <taktoa@gmail.com>
;;
;;; Homepage:   https://github.com/taktoa/parallax
;;
;;; Commentary:
;;+; This file contains the guildhall package specification for Parallax.
;;+;
;;+; Here's the EBNF+CPP specification of the package syntax, for reference:
;;+; @example
;;+; #define %sexpr(args) ("(", args, ")")
;;+; #define %many1(sym) (sym, {sym})
;;+; <pkg-list>           ::= %many1(<package>)
;;+; <package>            ::= ("package" (<name> <version>) {<property>})
;;+; <property>           ::=   %sexpr("synopsis",           <string>)
;;+;                        |  {%sexpr("description",        <string>)}
;;+;                        |   %sexpr("homepage",           <string>)
;;+;                        |  {%sexpr("depends",            <pkg-reference>)}
;;+;                        |  {%sexpr("libraries",         {<file-rule>})}
;;+;                        |  {%sexpr("programs",          {<file-rule>})}
;;+;                        |  {%sexpr("documentation",     {<file-rule>})}
;;+;                        |  {%sexpr("man",               {<file-rule>})}
;;+;                        |   %sexpr("installation-hook",  <hook-args>)
;;+;                        |   %sexpr(<property-name>,      <property-value>)
;;+; <pkg-reference>      ::= %sexpr(<pkg-name>)
;;+;                        | %sexpr(<pkg-name> <version-constraint>)
;;+; <version-constraint> ::= <version>
;;+;                        | %sexpr(<comparator> <version>)
;;+;                        | %sexpr(not  <version-constraint>)
;;+;                        | %sexpr(or  *<version-constraint>)
;;+;                        | %sexpr(and *<version-constraint>)
;;+; <comparator>         ::= "<="
;;+;                        | ">="
;;+;                        | "<"
;;+;                        | ">"
;;+; <version>            ::= %sexpr(%many1(<integer>))
;;+; <hook-args>          ::= %sexpr({<hook-option>}), <hook-body>
;;+; <hook-option>        ::= %sexpr("needs-source?", ".", <boolean>)
;;+; <hook-body>          ::= %sexpr("import", <hook-body-args>)
;;+; <hook-body-args>     ::= (%many1(<library-refs>), <defs>, <exprs>)
;;+; <category>           ::= %sexpr(<category-name>, {<file-rule>})
;;+; <user-defined>       ::= %sexpr(<property-name>, <property-value>)
;;+; <file-rule>          ::= <source>
;;+;                        | <source>, "->", <destination>
;;+;                        | %sexpr("exclude", {<source>})
;;+; <source>             ::= <string>
;;+;                        | %sexpr({<source>})
;;+;                        | %sexpr(":", {<sre>})
;;+; @end example
;;; Code:

(package (parallax (0 5 0))
  (depends (srfi-41))
  (synopsis "Monadic parser combinators for Guile.")
  (description
   "Parallax is a full-featured monadic parsing library for Guile, "
   "similar to the parsec library in Haskell.")
  (homepage "https://github.com/taktoa/parallax")
  (libraries
   ("parallax.scm" -> "parallax"))
  (documentation "README.md"
                 "INSTALL.md"
                 "STYLE.md"
                 "COPYING.md"
                 "doc/parallax.texi"))

;;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-
;;; File: examples/csv.scm
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
;;+; A parser for CSV files, as defined by RFC 4180.
;;+;
;;+; The grammar of the CSV language in EBNF+CPP is:
;;+; @example
;;+; #define %sep(a, b) (<a>, {<b>, <a>})
;;+; <csv>       = [<header>], %sep(<record>, <eol>), [<eol>]
;;+; <header>    = %sep(<name>, <comma>), <eol>
;;+; <record>    = %sep(<field>, <comma>)
;;+; <name>      = <cell>
;;+; <field>     = <cell>
;;+; <cell>      = <quoted> | <nonquoted>
;;+; <unquoted>  = {<printing> - (<comma> | <dq>)}
;;+; <quoted>    = <dq>, {(<printing> - <dq>) | <dq> <dq>}, <dq>
;;+; <comma>     = ","
;;+; <printing>  = ? In hexadecimal, any byte between 21 and 7E          ?
;;+; <dq>        = ? The double quote character (0x22)                   ?
;;+; <eol>       = ? Your favorite line ending (the RFC specifies CR+LF) ?
;;+; @end example
;;
;;; Code:

(define-module (parallax csv)
  #:use-module (parallax utils)
  #:use-module (parallax parse)
  #:export     (csv-line* csv*))

;;; ----------------------------------------------------------------------------
;;; ----------------------------------- Types ----------------------------------
;;; ----------------------------------------------------------------------------

(define-adt [Cell]
  [@cell (cell-val :: String)])

(define-adt [CSV]
  [@cells (get-cells :: Vector Cell)])

;;; ----------------------------------------------------------------------------
;;; ---------------------------------- Parsers ---------------------------------
;;; ----------------------------------------------------------------------------

;;; --------------------------------- Exported ---------------------------------

(sig csv-line* :: Parser Cell)
(sig csv*      :: Parser CSV)

(defparser csv-line*
  "Parser for a line in a CSV file."
  (&<$> @cell (&sep> cell* (char #\,))))

(defparser csv*
  "Parser for a CSV file."
  (&<$> (compose @cells list->vector)
        (&end> line* eol*)))

;;; ----------------------------------- Cells ----------------------------------

(sig nonquoted-cell* :: Parser String)
(sig quoted-cell*  :: Parser String)
(sig cell*         :: Parser String)

(defparser nonquoted-cell*
  "Parser for an nonquoted cell in a CSV file."
  (&** nonquoted-char*))

(defparser quoted-cell*
  "Parser for a quoted cell in a CSV file."
  (&<$> Cell (&=> [&<*  quote*]
                  [&<*> (&** quoted-char*)]
                  [&<*  quote*]))

(defparser cell*
  "A parser for a cell in a CSV file."
  (&</> quoted-cell* nonquoted-cell*))

;;; -------------------------------- Characters --------------------------------

(sig nonquoted-char* :: Parser Char)
(sig quoted-char*    :: Parser Char)

(defparser nonquoted-char*
  "Parser for allowed characters in a normal CSV cell."
  (&<!> (&</> delimiter* csv-eol*)))

(defparser quoted-char*
  "Parser for allowed characters in a CSV quoted cell."
  (&</> (&<!> csv-quote*) (try escaped-quote*)))

;;; ---------------------------------- Tokens ----------------------------------

(sig csv-quote*         :: Parser String)
(sig csv-delimiter*     :: Parser String)
(sig csv-eol*           :: Parser String)
(sig csv-escaped-quote* :: Parser Char)

(defparser csv-quote*
  "Parser for a CSV quote token."
  double-quote*)

(defparser csv-delimiter*
  "Parser for a CSV delimiter token."
  comma*)

(defparser csv-eol*
  "Parser for EOL in a CSV file."
  system-eol*)

(defparser csv-escaped-quote*
  "Parser for a quotation mark escaped inside a quote in a CSV file."
  (&>> csv-quote* csv-quote* (return #\")))

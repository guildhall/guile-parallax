;;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-
;;; File: examples/json.scm
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
;;+; This is an example of parsing JSON with Parallax.
;;
;;; Code:

(define-module (parallax <MODULE-NAME>)
  #:use-module (parallax utils)
  #:use-module (parallax parse)
  #:export     ())

;;; ----------------------------------------------------------------------------
;;; ----------------------------------- Types ----------------------------------
;;; ----------------------------------------------------------------------------

(define-adt [JSON]
  [json-string ([get-json-string :: String])]
  [json-number ([get-json-number :: Number])]
  [json-bool   ([get-json-bool   :: Bool])]
  [json-object ([get-json-object :: Map String JSON])]
  [json-array  ([get-json-array  :: Vector JSON])]
  [json-null   ()])

;;; ----------------------------------------------------------------------------
;;; ---------------------------------- Parsers ---------------------------------
;;; ----------------------------------------------------------------------------

;;; ---------------------------------- Strings ---------------------------------

(deftoken quote*
  "JSON string start/end token."
  "\"")

(deftoken escape*
  "JSON string escape token."
  "\\")

(defparser unicode*
  "Parser for a Unicode escape in a JSON string."
  &<$>(&#> 4 digit*))

(defparser json-string-escaped*
  "Parser for a character in a JSON string"
  (&>> backslash*
       (&/> (&$> "b"  #\backspace)
            (&$> "n"  #\newline)
            (&$> "f"  #\page)
            (&$> "r"  #\return)
            (&$> "t"  #\tab)
            (&$> "\\" #\\)
            (&$> "\"" #\")
            (&$> "/"  #\/)
            json-string-unicode*)))

(defparser json-string-char*
  "Parser for a character in a JSON string"
  (&/> json-string-escaped*
       (&!> (&/> json-string-escape*
                 json-string-quote*))))

(defparser json-string*
  "Parser for a JSON string"
  (&<$> (%. @json-string list->string)
        (&b/w> quote* quote* (many json-string-char*))))

(defparser json-number*
  "docstring"
  (&<$> (%. @json-number read-number) number*))

(define (read-number))

;; Code for parsing numbers with the reader
(define (pport-read-number pp)
  (let ([pos (pport-save pp)]
        [num (pport-read pp)])
    (match num
      ;; If the read returned a number, return
      [(Just n)  (Consumed (Ok n))]
      ;; Otherwise, fail and backtrack
      [(Nothing) (begin
                   (pport-rewind! port pos)
                   (Empty (Error)))])))

(defparser json-bool*
  "Parser for JSON boolean expressions."
  (&<$> @json-bool (&</> (&>> "true"  (return #t))
                         (&>> "false" (return #f)))))

(defparser json-null*
  "Parser for the JSON null object."
  (&$> "null" (json-null)))

(define (&<$> f p)
  (&>>= p (λ [x] (return (f x)))))

(defparser json-value*
  "A JSON value."
  (&</> json-number*
        json-object*
        json-array*
        json-bool*
        json-null*))

(define $p_value (&>> whitespace* $value))

(define json-field*
  (parser-do
   (k <- $p_string)
   (char #\:)
   whitespace*
   (v <- $p_value)
   (return (cons (list->string k) v))))

(define (p_series left p right)
  (&b/w> (parser-one (~> (char left)) whitespace*)
         (char right)
         (&sep> (parser-one (~> p) whitespace*)
                (parser-one (~> (char #\,)) whitespace*))))

(define %. compose)

(defparser json-array*
  "Parser for a JSON array."
  (&<$> (%. @json-array list->vector)
        (p_series #\[ $p_value #\])))

(defparser json-object*
  "Parser for a JSON object."
  (&<$> (%. @json-object) (p_series #\{ $p_field #\})))

(define text*
  )

;; parses json
(defparser json*
  "JSON text"
  (&<?> (parser-one whitespace* (&~> (&</> json-object* json-array*))) ))

;;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-
;;; File: parallax.scm
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
;; Monadic parser combinators for Guile.
;;
;;; Code:

(define-module (parallax)
  #:use-module (parallax parse))

(define-syntax re-export-modules
  (syntax-rules ()
    ((_ (mod ...) ...)
     (begin
       (module-use! (module-public-interface (current-module))
                    (resolve-interface '(mod ...)))
       ...))))

(re-export-modules (parallax parse))

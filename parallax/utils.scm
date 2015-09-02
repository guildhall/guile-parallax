;;; -*- mode: scheme; geiser-scheme-implementation: guile -*-
;;; File: parallax/utils.scm
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
;;+; Utilities used by the rest of the (parallax) libraries and examples.
;;
;;; Code:

(define-module (parallax utils)
  #:version    (0 5 0)
  #:use-module (scheme   documentation)
  #:use-module (language tree-il)
  #:use-module (ice-9    pretty-print)
  #:use-module (ice-9    match)
  #:use-module (ice-9    curried-definitions)
  #:use-module (srfi     srfi-26)
  #:export     (;; Macros
                sig
                ;; String functions
                string-empty?
                string-head
                string-tail))

;;; ----------------------------------------------------------------------------
;;; ----------------------------- Exported Macros ------------------------------
;;; ----------------------------------------------------------------------------

(define-syntax-rule (λ* (formals ...) body ...)
  (lambda* (formals ...) body ...))

;;; ------------------------------- Type system --------------------------------

(define-syntax sig (λ [stx] #'5))

;;; ----------------------------------------------------------------------------
;;; ---------------------------- Exported Functions ----------------------------
;;; ----------------------------------------------------------------------------

(define* (symlist? value)
  "Is the given @var{value} a homogeneous list of symbols?"
  (letrec ([syms? (λ [xs] (or (null? xs)
                              (and (symbol? (car xs))
                                   (syms?   (cdr xs)))))])
    (and (list? value) (syms? value))))

(define* (printf fmt #:rest args)
  "Print a formatted string to @code{(current-output-port)}"
  (apply format #t fmt args))

(define* (printfln fmt #:rest args)
  "Print a formatted string to @code{(current-output-port)}, with a newline."
  (apply printf fmt args)
  (newline))

(define* (cprintf fmt #:rest args)
  "Same as @code{printf}, except it prepends @code{\";; \"}."
  (apply printf (string-append ";; " fmt) args))

(define* (cprintfln fmt #:rest args)
  "Same as @code{printfln}, except it prepends @code{\";; \"}."
  (apply printfln (string-append ";; " fmt) args))

(define* (fmtstr fmt #:rest args)
  "Shortcut for @code{(format #f @var{fmt} @var{args} @var{...})}"
  (apply format #f fmt args))

(define* (err sym msg #:rest args)
  "Throw an error tagged with the function @var{sym} and the message @var{msg}."
  (apply error (fmtstr "~a: ~a" sym msg) args))

;;; ----------------------------- String functions -----------------------------


'(sig string-empty? :: String -> Bool)
'(sig string-head   :: String -> Char)
'(sig string-tail   :: String -> String)

(define* (string-empty? str)
  "Is the given string, @var{str}, equal to the empty string?"
  (string=? str ""))
(define* (string-head str)
  "Returns the first character of the given string, @var{str}."
  (string-ref str 0))
(define* (string-tail str)
  "Returns all but the first character in the given string, @var{str}."
  (substring str 1))

;;; SCRATCHPAD

(define* input-file
  "/home/remy/Documents/NotWork/Projects/Guile/guile-parsack/test.scm")

(define* input-sexpr
  (cadr (with-input-from-file input-file read)))

(define* (pam list proc) (map proc list))

(define* (list->set list)
  (let ([set (make-set)])
    (for-each (cut set-add! set <>) list)
    set))

(define* (unique list)
  (set->list (list->set list)))

(define* (id x) x)

(define* (set->list set #:key [mapping id])
  (hash-map->list (λ [k v] (mapping k)) set))

(define* (set-for-each proc set)
  (hash-for-each (λ [k v] (proc k)) set))

(define* (set-contains set elem)
  (hash-ref set elem))

(define* (make-set)
  (make-hash-table))

(define* (set-add! set elem)
  (hash-set! set elem #t))

(define* (expand-macros sexpr)
  (tree-il->scheme (macroexpand sexpr)))

(define* ((visit-sexpr func) tree)
  (if (list? tree)
      (func (map (visit-sexpr func) tree))
      (func tree)))

(define* (test-sexpr-visit value)
  'a)

(define* (test-sexpr tree)
  (let ([visit (match-lambda
                 [(? symbol? x) `(test-symbol  ,x)]
                 [(? sexpr? x)  `(test-sexpr  ,@x)]
                 [(? list? x)   `(test-list   ,@x)]
                 [x             `(test-other   ,x)])])
    ((visit-sexpr test-sexpr-visit) tree)))

(define* (sexpr? val)
  (or (symbol? val)
      (and (list?   val)
           (symbol? (car val)))))

(define* (get-variables sexpr)
  (let* ([vars  (make-set)]
         [visit (λ [s] (when (symbol? s) (set-add! vars s)))])
    ((visit-sexpr visit) (if (sexpr? sexpr) sexpr (list sexpr)))
    (set->list vars)))

(define* (get-bindings input)
  (letrec ([result  (make-set)]
           [recurse (match-lambda
                      [`(define* (,name . ,formals) . ,body)
                       (set-add! result name)]
                      [`(define* ,name ,rest)
                       (recurse `(define* (,name) . ,rest))]
                      [`(define . ,rest)
                       (recurse `(define* . ,rest))]
                      [`(define-syntax-rule . ,rest)
                       (recurse `(define* . ,rest))]
                      [_ '()])])
    (map recurse input)
    (set->list result)))

(define* (modify-sexpr input)
  (map modify-subtree input))

(define* (add1 x) (+ x 1))
(define* (sub1 x) (+ x 1))

(define* (process-let type bindings body)
  (cprintfln "Test: type:     ~s" type)
  (cprintfln "Test: bindings: ~s" bindings)
  (cprintfln "Test: body:     ~s" body)
  `(,type ,bindings ,body))

(define* (remove-formals-test remove subtree)
  (let ([recurse (cut remove-formals-test remove <>)])
    (match subtree
      [`(define-module . _) '()]
      [`(sig           . _) '()]
      [`(def-adt       . _) '()]
      [`(def-instance  . _) '()]
      [`(def-parser    . _) '()]
      [`(let ,(bindings *** (...)) ,body)
       (process-let 'let bindings body)]
      [`(,func . ,args)
       (cons func (map recurse args))]
      [otherwise otherwise])))

(define* (remove-formals formals body)
  (let ([body-set    (make-set)]
        [formals-set (make-set)]
        [result-set  (make-set)])
    (cprintfln "DBG: ~s" formals)
    ((visit-sexpr (λ [b] (when (symbol? b) (set-add! body-set b)))) body)
    (for-each (cut set-add! formals-set <>) formals)
    (set-for-each (λ [b] (unless (set-contains formals-set b)
                           (set-add! result-set b)))
                  body-set)
    (set->list result-set)
    ))

(define* (sort-symbols list)
  (map string->symbol (sort (map symbol->string list) string<?)))

(define* (modify-subtree input)
  (match input
    [`(define* (,name . ,formals) . ,body)
     `((definition ,name ,(get-variables body)))]
    [`(define* ,name ,rest)
     (modify-sexpr `(define* (,name) . ,rest))]
    [`(define . ,rest)
     (modify-sexpr `(define* . ,rest))]
    [`(define-syntax-rule . ,rest)
     (modify-sexpr `(define* . ,rest))]
    [_ '()]))

(define* (modify-sexpr input)
  (map modify-subtree input))

(define* pp pretty-print)

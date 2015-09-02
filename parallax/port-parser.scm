;;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-
;;; File: parallax/port-parser.scm
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
;;+; An implementation of a monadic parser combinator library using ports.
;;
;;; Code:

(define-module (parallax port-parser)
  #:version    (0 5 0)
  #:use-module (parallax utils)
  #:export     (@consumed @empty @ok @error
                exn:fail:parsack
                err err*
                parse-error
                parse-source
                return
                satisfy
                one-of
                none-of
                one-of-strings
                one-of-strings-ci
                &>>= &>> &</> &<doc>
                option option-maybe optional
                try look-ahead not-followed-by
                many many1 skip-many skip-many1
                many-till many1-till many-until many1-until
                separate separate1 end between
                &<!> &<@> &<#>
                char char-ci
                any-char* letter* digit* alphanum* hex-digit* identifier*
                byte bytestring
                space* spaces* newline* tab* eof* eol*
                string string-ci
                parse parse-result
                parser-compose parser-seq parser-cons parser-one
                choice))

;;; ----------------------------------------------------------------------------
;;; ---------------------------- Exported Functions ----------------------------
;;; ----------------------------------------------------------------------------


(define documentation-prop (make-object-property))

(define-syntax-rule (def-adt ...) '())

(define-syntax-rule (thunk e) (λ [] e))

(define-syntax-rule (def-parser name docstring body ...)
  (begin
    (define name (label-parser (quote name) (begin body ...)))
    (set! (documentation-prop name) docstring)))

;; (def-parser identifier*
;;   "docstring"
;;   (many1 (&</> letter* digit* "_")))

(define* (macro-ref macro-name)
  "Returns the macro object associated with the given (quoted) macro name."
  (module-ref (current-module) macro-name))

(define-syntax def-syntax
  (λ (stx)
    "A cleaner way to define macros."
    (syntax-case stx []
      [(_ (kw sv) doc body ...) #'(let ([m (macro-ref (quote kw))])
                                    (define-syntax kw
                                      (λ (sv) doc body ...))
                                    (set! (documentation-prop m) doc))]
      )))

(define (lookup-macro-documentation name)
  "Get the documentation for a given quoted macro name."
  (let ([mcr (module-ref (current-module) name)])
    (if (macro? mcr)
        (procedure-documentation (macro-transformer mcr))
        (error "Current module does not contain a macro named " name))))

;; (def-syntax (macro-documentation stx)
;;   "Get the documentation for a given unquoted macro name."
;;   (syntax-case stx []
;;     [(_ macro-name) (lookup-macro-documentation (quote macro-name))]))

(define* (simple-type-of value)
  "Get the type of the given simple value."
  (match value
    [(? boolean?)    'Bool]
    [(? symbol?)     'Symbol]
    [(? keyword?)    'Keyword]
    [(? number?)     '[Num a => a]]
    [(? char?)       'Char]
    [(? char-set?)   'CharSet]
    [(? string?)     'String]
    [(? box?)]
    [(? bitvector?)  '[Vector Bit]]))

(define* (complex-type-of value)
  "Get the type of the given complex value."
  (match value
    [`(cons ,x ,y) `[Pair ,(type-of x) ,(type-of y)]]
    [(? boolean?)  'Bool]
    [(? boolean?)  'Bool]
    [(? boolean?)  'Bool]))


;;;;;;;;;;;;;;;;;;;;;;;;; Parsec (with error messages) ;;;;;;;;;;;;;;;;;;;;;;;;;
;; implements monadic combinators for LL parsing

;; p is an InputPort when (input-port? p)

;; A [Parser X] is a function: InputPort -> [Result X]

;; Naming conventions:
;; Parsers use a *-suffixed name.
;; Parser combinators have hyphenated names

;; Two different factors may be used to combine parsers:
;; - whether a parser consumed input, ie returns @consumed and not empty
;; - whether a parser succeeds, ie returns an @ok and not an @error
;; Some combinators use only one of the factors when determining how to combine
;; parsers; others use both.

;; A [Result X] is one of:
;; - (@consumed a)
;; - (@empty a)
;; NOTE: Cannot use #f as parse result because some parsers want to produce #f, eg json

(def-adt [Reply a]
  [@ok    (get-ok :: a)]
  [@error])

(def-adt [Result a]
  [@consumed (get-consumed :: Reply a)]
  [@empty    (get-empty    :: Reply a)])

(define* (negate f)
  "docstring"
  (λ (x) (not (f x))))

(define* (curry f x)
  "docstring"
  (λ args (apply f x args)))

;; global parameters ----------------------------------------------------------
;; current-unexpected : [Parameter String]
;; The current unexpected char.

(define current-unexpected% (make-parameter ""))

(define* (reset!-unexpected)
  "docstring"
  (current-unexpected% ""))

(define* (get-unexpected)
  "docstring"
  (current-unexpected%))

(define* (set!-unexpected string-thunk)
  "docstring"
  (current-unexpected% string-thunk))

;; current-expected : [List [Thunk String]]
;; The current expected chars.
(define* current-expected (make-parameter '()))

(define* (reset!-expected)
  (current-expected% null))

(define* (get-expected)
  (current-expected%))

(define* (set!-expected exps)
  (current-expected% exps))

(define* (merge!-expected exps1 exps2)
  (set!-expected (append exps1 exps2)))

(define* (prepend-expected! exp)
  (set!-expected (cons exp current-expected)))

(define* (append!-expected exps)
  (set!-expected (append exps current-expected)))

;; errors ---------------------------------------------------------------------
(define* (err expected)
  (λ [in]
    (set!-unexpected (thunk (port->string in)))
    (set!-expected (list expected))
    (@empty (@error))))

(def-parser err*
  "Parser that always throws an error."
  (err ""))

(define-syntax-rule (parsack-error msg)
  (thunk (error (string-append "parse ERROR: " msg))))

;; A Pos is a (Pos line col ofs), numbers are 1-based
(struct Pos (line col ofs) #:transparent)
(define* parse-source (make-parameter #f)) ;; not in Pos for efficiency
(define* (format-pos p)
  (match* (p (parse-source))
          [((Pos line col ofs) (? path-string? src))
           (format "~a:~a:~a:~a" src line col ofs)]
          [((Pos line col ofs) _)
           (format "~a:~a:~a" line col ofs)]))

;; creates a Parser that consumes no input and returns x
(define* (return x)
  (λ [in]
    (reset!-unexpected)
    (reset!-expected)
    (@empty (@ok x))))

;; creates a parser that consumes 1 char or byte (ie one unit of input)
;; if it satisfies predicate p?
(define* (satisfy p? #:key [read read-char] [peek peek-char])
  (λ [in]
    (define c (peek in))
    (cond
     [(eof-object? c)
      (set!-unexpected (thunk "end of input"))
      (reset!-expected)
      (@empty (@error))]
     [(p? c)
      (reset!-unexpected)
      (reset!-expected)
      (@consumed (@ok (read in)))] ; commit peeked
     [else
      (set!-unexpected (thunk (cond [(char? c) (mk-string c)]
                                    [(byte? c) (byte->str c)]
                                    [else (~v c)])))
      (reset!-expected)
      (@empty (@error))])))

(define* (of-string ? exps)
  (λ [in]
    (match ((satisfy ?) in)
      [(and result (or (@consumed (@error))
                       (@empty    (@error)))) ; error
       (cons!-expected exps)
       result]
      [ok ok])))

(define* (one-of str)
  (of-string (make-char-in-string? str)
             (thunk (string-append "one of: " (str->strs str)))))

(define* (none-of str)
  (of-string (negate (make-char-in-string? str))
             (thunk (string-append "none of: " (str->strs str)))))

(define* (make-char-in-string? str)
  ""
  ;; `(for/or ([c (in-string str)]))` is slow. A precomputed `seteqv`
  ;; surprisingly isn't that much better. However a precomputed
  ;; `hasheqv` IS significantly faster.
  (let ([ht (for/hasheqv ([c (in-string str)]) (values c #t))])
    (λ [c] (hash-ref ht c #f))))

(define* (str->strs str)
  (format-exp (map mk-string (string->list str))))

(define* (one-of-strings #:rest ss)
  (&<doc> (choice (map (compose1 try string) ss))
        (fmt "(~a)" (joinmap ", " ~s ss))))

(define* (one-of-strings-ci #:rest ss)
  (&<doc> (choice (map (compose1 try string-ci) ss))
        (fmt "%ci(~a)" (joinmap ", " ~s ss))))

(sig joinmap :: IsString s => s -> (a -> s) -> s)
(define* (joinmap del func list)
  (string-join (map )))

(sig ~a :: Show v => v -> String)
(define* ~a
  (cut fmt "~a" <>))

(sig ~s :: Show v => v -> String)
(define* ~s
  (cut fmt "~s" <>))

;; &>>= : [Parser X] ([Result X] -> [Parser Y]) -> [Result Y]
;; Creates a Parser from a Parser p and a continuation f for p's result.
;; - if p succeeds but does not consume input, then f determines result
;; - if p succeeds and consumes input, return @consumed with reply from f
;; - if p fails, do not continue with f


(define* (&>>= p f)
  (λ [in]
    (match (p in)
      [(@empty (@ok x))
       (let ([saved-expected (get-expected)])
         (match ((f x) in)
           [(@empty y) (begin
                         (append!-expected saved-expected)
                         (@empty y))]
           [consumed   consumed]))]
      [(@consumed (@ok x))
       (let ([saved-expected (get-expected)])
         (@consumed
          (match ((f x) in)
            [(@empty (@error)) (begin
                                 (append!-expected saved-expected)
                                 (@error))]
            [(@consumed reply) reply]
            [(@empty reply)    reply])))]
      [err err])))

(define* (&>> p q) (&>>= p (λ [_] q)))

;; <|> choice combinator
;; first tries to parse with p, only tries q if p does not consume input
;; - if p does not consume input and errors, return result of q
;; - if p does not consume input but returns result, then use that result unless q consumes
;; thus, &</> implements "longest match"
(define* (&</>2 p q)
  (λ [in]
    (match (p in)
      [(@empty (@error))
       (let ([saved-expected (get-expected)])
         (match (q in)
           [(@empty x)
            (append!-expected saved-expected)
            (@empty x)]
           [consumed consumed]))]
      [(@empty (@ok x))
       (let ([saved-expected (get-expected)])
         (match (q in)
           [(@empty _)
            (append!-expected saved-expected)
            (@empty (@ok x))]
           [consumed consumed]))]
      [consumed consumed])))

;; assumes (length args) >= 1
(define* (&</> #:rest args)
  (foldl (λ [p acc] (&</>2 acc p)) (car args) (cdr args)))

;; short-circuiting choice combinator
;; only tries 2nd parser q if p errors and consumes no input
;; differs from &</> in the case where p returns (@empty (@ok ...))
;; - &</>: parse with q
;; - &<@>: stops
(define* (&<@>2 p q)
  (λ [in]
    (match (p in)
      [(@empty (@error))
       (let ([saved-expected (get-expected)])
         (match (q in)
           [(@empty x) (begin
                         (append!-expected saved-expected)
                         (@empty x))]
           [consumed  consumed]))]
      [res res])))

;; assumes (length args) >= 2
(define* (&<@> fst snd #:rest rest)
  (foldl (λ [p acc] (&<@>2 acc p)) fst (cons snd rest)))

(define* (option x p) (&</> p (return x)))
(define* (option-maybe p) (option #f p))
(define* (optional p)
  (&</> (&>> p (return null))
        (return null)))

;; tries to parse with p but backtracks and does not consume input if error
(define* (try p)
  (λ [in]
    (define-values (r c pos) (port-next-location in))
    (define byte-pos (file-position in))
    (match (p in)
      [(@consumed (@error))
       (file-position in byte-pos) ; backtrack
       (set-port-next-location! in r c pos)
       (@empty (@error))]
                                        ; dont need to back track if Empty-Error
      [other other])))

;; Parse p and return the result, but don't consume input.
(define* (look-ahead p)
  (λ [in]
    (define-values (r c pos) (port-next-location in)) ; save current loc
    (define byte-pos (file-position in))
    (match (p in)
      [(@consumed (@ok result))
       (file-position in byte-pos) ; backtrack
       (set-port-next-location! in r c pos)
       (set!-unexpected (thunk (port->string in)))
       (@empty (@ok result))]
      [res res])))

;; converts intermediate parse result to string -- for error purposes
;; Note: Efficiency of this matters, do dont call until throwing the exception
(define* (result->str res)
  (cond [(char? res) (mk-string res)]
        [(and (list? res) (andmap char? res)) (list->string res)]
        [else res]))

;; fails (and does not consume input) if p succeeds and consumed input,
;; otherwise parse with q
(define* (&<!> p #:optional [q any-char*])
  (λ [in]
    (define-values (r c pos) (port-next-location in)) ; saved location
    (define byte-pos (file-position in))
    (define res (p in))
    (file-position in byte-pos) ; always backtrack
    (set-port-next-location! in r c pos)
    (match res
      [(@consumed (@ok res))
       (set!-unexpected (thunk (result->str res)))
       (set!-expected (list (thunk (format "not: ~a" (result->str res)))))
       (@empty (@error))]
      [_ (q in)])))

;; succeeds when p fails; does not consume input
;; differs from &<!> in that there is no second parser to try
(define* (not-followed-by p)
  (λ [in]
    (define-values (r c pos) (port-next-location in)) ; saved locaition
    (define byte-pos (file-position in))
    (define res (p in))
    (file-position in byte-pos) ; always backtrack; never consume
    (set-port-next-location! in r c pos)
    (match res
      [(@consumed (@ok res))
       (set!-unexpected (thunk (result->str res)))
       (set!-expected (list (thunk (format "not: ~a" (result->str res)))))
       (@empty (@error))]
      [_ (reset!-unexpected)
         (reset!-expected)
         (@empty (@ok null))])))

;; parse with p 0 or more times
;; some notes:
;; - default #:till can be (return <anything>), just needs to not consume input
;; - using many with #:or &<@> and the default #:till will immediately return
;;   empty result without consuming input
(define* (many parser
               #:key
               [till (return 0)]
               [or   &</>])
  "Parse with @var{parser} zero or more times.
Keyword arguments:
* @lisp{#:till} :: defaults to @lisp{(return 0)}, represents
* @lisp{#:or}   :: defaults to @lisp{&</>}, represents "
  (&</> (&>> till (return null))
        (parser-cons parser
                     (many parser
                           #:till till
                           #:or or))))

(define* (many1 parser
                #:key
                [till (return null)]
                [or   &</>])
  "Parse with @var{parser} one or more times."
  (parser-cons p (many p #:till end #:or <or>)))

(define* (&/null> p)
  (&</> p (return null)))

(define* &.> parser-compose)

(define* (skip-many p)
  "docstring"
  (&/null> skip-many1))

(define* (skip-many1 p)
  "docstring"
  (&<doc> (&.> p (skip-many p))
        (fmt "(~a {~a})" )))

;; applies parser p zero or more times until parser end succeeds
(define* (many-till p end #:key [or &</>])
  (many p #:till end #:or or))

;; applies parser p one or more times until parser e succeeds
(define* (many1-till p e #:key [or &</>])
  (parser-cons p (many-till p e #:or or)))

;; many-until = many-till #:or &<@>
(define* (many-until p e)
  "docstring"
  (many-till p e #:or &<@>))
(define* (many1-until p e)
  (many1-till p e #:or &<@>))

(define* (separate1 p sep)
  (parser-cons p (many (&>> sep p))))
(define* (separate p sep)
  "docstring"
  (&</> (separate1 p sep)
        (return null)))

(define* (end p e)
  "docstring"
  (many (parser-one (~> p) e)))

(define* (between open close p)
  "docstring"
  (parser-one open (~> p) close))

(define* &b/w> between)

;; Creates a Parser that parses with p, using exp as the expected input.
;; TODO: why is exp not merged?
(define* (&<doc> p exp)
  (λ [in]
    (match (p in)
      [(@empty x) (begin
                    (set!-expected (list exp))
                    (@empty x))]
      [other      other])))

(define (label-parser name parser)
  "docstring"
  (&<doc> parser (symbol->string name)))

;; creates a parser that parses char c
(define* (char c)
  "docstring"
  (&<doc> (satisfy (curry char=? c))
          (mk-string c)))

(define* (byte->str b)
  "docstring"
  (bytes->string/utf-8 (bytes b)))

(define* (byte b)
  "docstring"
  (&<doc> (satisfy (curry = b) #:read read-byte #:peek peek-byte)
          (byte->str b)))

(define* (char-ci c)
  "docstring"
  (&<doc> (satisfy (curry char-ci=? c))
          (format #t "(~a | ~a)" (char-upcase c) (char-downcase c))))

(def-parser letter*
  "docstring"
  (&<doc> (satisfy char-alphabetic?)
        "letter"))

(def-parser digit*
  "docstring"
  (&<doc> (satisfy char-numeric?)
        "digit"))

(def-parser alphanum*
  "docstring"
  (&<doc> (satisfy (λ [c] (or (char-alphabetic? c) (char-numeric? c))))
        "(<letter> | <digit>)"))

(def-parser hex-digit*
  "Parser that is satisfied by a case-insensitive hexadecimal digit.
This parser corresponds to the following regular expression: @code{[0-9a-fA-F]}"
  (&</> digit*
        (one-of "abcdef")
        (one-of "ABCDEF")))

(def-parser whitespace*
  "Parser that is satisfied by a whitespace character."
  (satisfy char-whitespace?))

(def-parser many-whitespace*
  "Parser that skips zero or more whitespace characters."
  (skip-many space*))

(def-parser any-char*
  "Parser that is satisfied by any character."
  (&<doc> (satisfy (λ [_] #t))
        "any character"))

(def-parser newline*
  "docstring"
  (&<doc> (char #\newline) "new-line"))

(def-parser tab*
  "docstring"
  (&<doc> (char #\tab) "tab"))

;; Consume and return a string for which the parser succeeds on each
;; character.
(define* (string* str p)
  "docstring"
  (chars (string->list str) p))

;; Parser p must parse successfully with each c in cs
(define* (chars cs p)
  "docstring"
  (if (null? cs)
      (return null)
      (parser-cons (p (car cs)) (chars (cdr cs) p))))

(define* (string str) ;case sensitive
  "docstring"
  (string* str char))

(define* (string-ci str) ;case insensitive
  "docstring"
  (string* str char-ci))

(define* (bytestring bstr)
  "docstring"
  (chars (bytes->list bstr) byte))

(def-parser eof*
  "Parser that only succeeds on empty input (i.e.: when we reach end-of-file)."
  (λ [in] (match (peek-char in)
            [(? eof-object?) (begin (reset!-unexpected)
                                    (reset!-expected)
                                    (@empty (@ok null)))]
            [_               (begin (set!-unexpected (thunk "non-empty input"))
                                    (reset!-expected)
                                    (@empty (@error)))])))

(def-parser eol*
  "docstring"
  (&</> (try (string "\n\r"))
        (try (string "\r\n"))
        (try (string "\n"))
        (try (string "\r"))))

(def-parser identifier*
  "docstring"
  (many1 (&</> letter* digit* "_")))

(define* (frc e)
  "docstring"
  (if (procedure? e) (e) e))

(define* (format-exp exp)
  "docstring"
  (string-join (map frc exp) ", " #:before-last " or "))

;; An Input is one of:
;; - String,
;; - Path p, where (path? p) = #t
;; - InputPort in, where (input-port? in) = #t

;; parse : ParseInput p => Parser a -> p -> Result a
;; Raises exception if p does not succeed, i.e.: it returns @code{@@ok}
;; errors have to be printed ~s, otherwise newlines get messed up

(def-predicate-class [InputPort p] input-port?)
(def-predicate-class [IsString s]  string?)
(def-predicate-class [Path p]      path?)

(define* (flip f)
  "Flip the arguments of a dyadic function."
  (λ* (x y) (f y x)))

(def-typeclass [ParseInput a]
  [with-parse-input-port :: InputPort p => (p -> b) -> a -> b])

(def-instance [IsString s  => ParseInput s]
  [with-parse-input-port = (flip with-input-from-string)])
(def-instance [Path p      => ParseInput p]
  [with-parse-input-port = (flip with-input-from-file)])
(def-instance [InputPort p => ParseInput p]
  [with-parse-input-port = id])

(define* (parse-error input-port)
  "docstring"
  (let-values ([[r c pos] (port-next-location input-port)])
    (parsack-error
     (format "at ~a:~%unexpected: ~s~%  expected: ~s"
             (format-pos (Pos r (add1 c) pos))
             (frc        (get-unexpected))
             (format-exp (get-expected))))))

(sig parse :: ParseInput i => Parser a -> i -> Result a)
(define* (parse p #:optional [i (current-input-port)])
  "Parse an input (by default, the current input port) with the given parser.
Raises an exception if @var{p} does not succeed, i.e.: it returns @code{@@ok}."
  (with-parse-input-port
   (λ (input-port)
     (port-count-lines! input-port)
     (reset!-unexpected)
     (reset!-expected)
     (match (parser input-port)
       [(or (@empty    (@error))
            (@consumed (@error))) ]
       [ok                      ok]))))

(define* (parse-result p s)
  (match (parse p s)
    [(@consumed (@ok parsed)) parsed]
    [(@empty    (@ok parsed)) parsed]
    [x (parsack-error (~v x))]))

(def-syntax (parser-compose stx)
  "docstring"
  (syntax-case stx [<-]
    [(_ p)              #'p]
    [(_ (x <- p) e ...) #'(&>>= p (λ (x) (parser-compose e ...)))]
    [(_ q e ...)        #'(&>>= q (λ (x) (parser-compose e ...)))]))

(define* (add-bind-seq stx)
  (syntax-case stx [~]
    [(~ p) #'p]
    [q     #`(#,(generate-temporary) <- q)]))

(def-syntax (parser-seq stx)
  "docstring"
  (syntax-case stx (~)
    [(_ p:expr ...
        (~optional (~seq #:combine-with combine:expr) #:defaults ([combine #'list])))
     (with-syntax ([(new-p ...) (map add-bind-seq (syntax->list #'(p ...)))])
       (syntax-case #'(new-p ...) [<-]
         [(~and ((~or (x <- q1) q2) ...) (q ...))
          #'(parser-compose q ... (return (combine x ...)))]))]))

(define-syntax-rule (parser-cons x y)
  "docstring"
  (parser-seq x y #:combine-with cons))

(define-syntax-rule (&<:> x y)
  "An alias for @lisp{parser-cons}."
  (parser-cons x y))

(define* (add-bind-one stx)
  (syntax-case stx [~>]
    [(~> p) #'p]
    [q      #`(~ q)]))

(define-syntax (parser-one stx)
  (syntax-case stx [~>]
    [(_ (~and (~seq (~or (~once (~> q1:expr)
                                #:name     "return parse (wrapped with ~>)"
                                #:too-many "too many parses to return (wrapped with ~>)"
                                #:too-few  "missing return parse (wrapped with ~>)")
                         (~not (~> q2:expr))) ...)
              (~seq p:expr ...)))
     (with-syntax ([(new-p ...) (map add-bind-one (syntax->list #'(p ...)))])
       #'(parser-seq new-p ... #:combine-with (λ (x) x)))]))

(define* (choice ps) (apply &</> ps))

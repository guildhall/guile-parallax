;;; -*- mode: scheme; geiser-scheme-implementation: guile; coding: utf-8 -*-
;;; File: examples/http-request.scm
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
;;+; A parser for HTTP requests, as defined by RFC 2616.
;;+;
;;+; The grammar of HTTP requests in EBNF+CPP is:
;;+; @example
;;+; #define %sep(a, b) (<a>, {<b>, <a>})
;;+; <csv>       = <request-line> { <crlf>}
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

(define-module (parallax http)
  #:use-module (parallax utils)
  #:use-module (parallax parse)
  #:export     (request*))

(define-type-alias URL String)
(define-type-alias Headers (Map HeaderKey String))

(define-adt [HTTPRequest]
  [@request ([get-method  :: Method]
             [get-url     :: URL]
             [get-headers :: Headers]
             [get-body    :: String])])

(define-adt [Method]
  [@OPTIONS ()]
  [@GET     ()]
  [@HEAD    ()]
  [@POST    ()]
  [@PUT     ()]
  [@DELETE  ()]
  [@TRACE   ()]
  [@CONNECT ()])

;; FIXME(taktoa): perhaps we should namespace these
;; FIXME(taktoa): these constructors should take typed values
;;                rather than just being mapped to strings.
(define-adt [HeaderKey]
  [@ACCEPT              ()]
  [@ACCEPT-CHARSET      ()]
  [@ACCEPT-ENCODING     ()]
  [@ACCEPT-LANGUAGE     ()]
  [@AUTHORIZATION       ()]
  [@EXPECT              ()]
  [@FROM                ()]
  [@HOST                ()]
  [@IF-MATCH            ()]
  [@IF-MODIFIED-SINCE   ()]
  [@IF-NONE-MATCH       ()]
  [@IF-RANGE            ()]
  [@IF-UNMODIFIED-SINCE ()]
  [@MAX-FORWARDS        ()]
  [@PROXY-AUTHORIZATION ()]
  [@RANGE               ()]
  [@REFERER             ()]
  [@TE                  ()]
  [@USER-AGENT          ()]
  [@EXPERIMENTAL        ()])

(define field-char*
  (&</> letter*
        digit*
        (one-of "-_")))

(define-syntax-rule (http-parser-cons x y)
  (parser-seq x y #:combine-with (compose list->string cons)))

(define field-name*
  (http-parser-cons letter* (many field-char*)))

(define continuation*
  (parser-cons (>> (many1 (one-of " \t")) (return #\space)) contents*))

(define not-eol* (none-of "\r\n"))

(sig cr*   :: Parser ())
(sig lf*   :: Parser ())
(sig crlf* :: Parser ())

(defparser cr*
  "Parser for a carriage return character."
  "\r")

(defparser lf*
  "Parser for a line feed character."
  "\n")

(defparser crlf*
  "Parser for a carriage return followed by a line feed."
  (>> cr* lf*))

(defparser contents*
  "Parser for the contents of"
  (parser-seq
   (parser-one (~> (many1 not-eol*)) crlf*)
   (&</> continuation* (return null))
   #:combine-with (compose list->string append)))

(define header*
  (parser-cons fieldName* (parser-compose (char #\:) spaces* contents*)))

(define headers* (manyTill header* crlf*))

(define url*
  (parser-one
   (optional (char #\/))
   (~> (many-till not-eol* (try (parser-one (~> (string " HTTP/1.")) (one-of "01")))))
   crlf*))

(define (q name ctor body)
  (&.>
   (m <- (&> (string name) " " (return ctor)))
   (u <- url*)
   (hs <- p_headers*)
   (b <- body)
   (return (HttpRequest m (list->string u) hs (and b (list->string b))))))

(defparser extension-method*
  "docstring"
  parser-fail)

(defparser method*
  "docstring"
  (&</> [&$> "OPTIONS" (@OPTIONS)]
        [&$> "GET"     (@GET)    ]
        [&$> "HEAD"    (@HEAD)   ]
        [&$> "POST"    (@POST)   ]
        [&$> "PUT"     (@PUT)    ]
        [&$> "DELETE"  (@DELETE) ]
        [&$> "TRACE"   (@TRACE)  ]
        [&$> "CONNECT" (@CONNECT)]
        extension-method*))

(defparser header-char*
  "docstring"
  ())

(defparser header*
  "docstring"
  (&</> [&$> "Accept"              (@ACCEPT)]
        [&$> "Accept-Charset"      (@ACCEPT-CHARSET)]
        [&$> "Accept-Encoding"     (@ACCEPT-ENCODING)]
        [&$> "Accept-Language"     (@ACCEPT-LANGUAGE)]
        [&$> "Authorization"       (@AUTHORIZATION)]
        [&$> "Expect"              (@EXPECT)]
        [&$> "From"                (@FROM)]
        [&$> "Host"                (@HOST)]
        [&$> "If-Match"            (@IF-MATCH)]
        [&$> "If-Modified-Since"   (@IF-MODIFIED-SINCE)]
        [&$> "If-None-Match"       (@IF-NONE-MATCH)]
        [&$> "If-Range"            (@IF-RANGE)]
        [&$> "If-Unmodified-Since" (@IF-UNMODIFIED-SINCE)]
        [&$> "Max-Forwards"        (@MAX-FORWARDS)]
        [&$> "Proxy-Authorization" (@PROXY-AUTHORIZATION)]
        [&$> "Range"               (@RANGE)]
        [&$> "Referer"             (@REFERER)]
        [&$> "TE"                  (@TE)]
        [&$> "User-Agent"          (@USER-AGENT)]
        [&*> header-char*]experimental-header*))

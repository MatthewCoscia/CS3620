#lang racket
(require (for-syntax syntax/parse))

(define-syntax (p-for stx)
  (syntax-parse stx
    [(_ id (~literal in) seq body ...)
     #'(for ([id seq]) body ...)]
    [(_ id (start end) body ...)
     #'(for ([id (in-range start end)]) body ...)]))

(define (range x)
  (in-range x))

(p-for num in (range 5)
        (displayln (* num num)))

(define (print msg)
  (displayln msg))

(define (append-of lst item)
  (append lst (list item)))

(define (length-of lst)
  (length lst))

(define (reverse-of lst)
  (reverse lst))
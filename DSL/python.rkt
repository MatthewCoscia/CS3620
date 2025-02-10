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

(define (print msg)
  (displayln msg))

(for ([x (in-range 1 6)])
    (displayln x))
;; converts to
(p-for num in (range 5)
        (print (* num num)))
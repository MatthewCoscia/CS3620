#lang racket
(require "core-streams.rkt")

(define data1 (list
  (hash 'x 1 'y 10)
  (hash 'x 2 'y 10)
  (hash 'x 1 'y 20)))

(define data2 (list
  (hash 'x 1 'z "A")
  (hash 'x 2 'z "B")
  (hash 'x 3 'z "C")))

;; Build QueryResults with from
(define qr1 (from data1))
(define qr2 (from data2))

;; Example 1: Using join/hash
(define joined
  (query/rows
    qr1
    (join/hash qr2 'x 'x)))

;; Example 2: Using aggregate/multi with multiple group columns
(define multi-agg
  (query/rows
    qr1
    (aggregate/multi
      '((x #:using count)
        (y #:using max))
      #:by '(x)))) ; grouping by 'x here is contrived, but for illustration

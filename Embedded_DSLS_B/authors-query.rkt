#lang racket

(require "tables.rkt" "core-streams.rkt")

(define authors (load-table "authors.csv"))
(define articles (load-table "articles.csv"))

;; Find the titles of articles written by Fiona Brown.
(query/rows
  (from articles)
  (join (from authors #:qualify 'author)
    'author-id 'author.id)
  (where (lambda (row)
           (equal? (hash-ref row 'author.name) "Fiona Brown")))
  (select 'title))

;; Count the number of articles written by each author.
(query/rows
  (from articles)
  (join (from authors #:qualify 'author)
    'author-id 'author.id)
  (aggregate 'title #:using count #:by 'author.name))







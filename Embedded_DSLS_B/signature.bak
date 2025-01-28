#lang racket

;; A Table is a (Listof Row)
;;   All the rows in a table have the same set of columns.

;; A QueryResult is a
(struct query-result [data])
 
;; A ColName is a Symbol
;; A Row is a (HashOf ColName Any)
;;   Note: Although the `load-table` operation only brings in data as strings,
;;         in general our DSL should support any Racket value as data in rows.
;;         We will assume that data used with the DSL can be usefully compared
;;         with `equal?`.

;; A Clause is a (QueryResult -> QueryResult)

;; QueryResult, Clause ... -> Table
;; Compose a query that transforms the initial query-result (usually produced by `from`),
;; applying the transformation defined by each clause in the order they are provided.
;; Extract the result of the query as a table.
(define (query/rows query-result . clauses) (error 'query/rows "Not implemented"))

;; QueryResult, Clause ... -> QueryResult
;; Compose a query that transforms the initial query-result (usually produced by `from`),
;; applying the transformation defined by each clause in the order they are provided.
(define (query query-result . clauses) (error 'query "Not implemented"))

;; Table [#:prefix Symbol] -> QueryResult
;; Create a QueryResult from a table of data. If a prefix is specified,
;; every column name `name` is transformed to `prefix.name`.
(define (from table #:qualify [prefix #f]) (error 'from "Not implemented"))

;; (Row -> Boolean) -> Clause
;; Filter, keeping only rows that match the predicate.
(define (where keep?) (error 'where "Not implemented"))

;; ColName ... -> Clause
;; Keep only the columns specified. Raises an error if a specified
;; column is absent from the data.
(define (select . col-names) (error 'select "Not implemented"))
  
;; QueryResult, ColName, ColName -> Clause
;; An inner join of the running query-result with the argument
;; query-result, on the column `col1` of the running query-result
;; and the column `col2` of the argument query-result.
;; Rows in the result should have the union of columns in the inputs.
;; Raises an error if there are any conflicting column names between
;; the two inputs.
(define (join qr2 col1 col2) (error 'join "Not implemented"))

;; QueryResult, Integer -> Clause
;; Retain only the first `n` rows of the query result. If the running
;; query result already has `n` or fewer rows, the limit has no effect.
(define (limit n) (error 'limit "Not implemented"))

;; -> Clause
;; Removes duplicate rows from the query result.
(define (distinct) (error 'distinct "Not implemented"))

;; An Aggregator is a (Any ... -> Any)

;; ColName, #:using Aggregator, #:by ColName -> Clause
;;  Groups rows by `by-col-name` and applies the aggregator to the list of 
;;  values of `col-name` in each group to compute the new value of `col-name`.
;;
;;  The result only has the `col-name` and `by-col-name` columns.
;;
;;  Raises an error if the data is missing either the `col-name` or `by-col-name`
;;  columns.
;;
;; Example:
;; (query/rows
;;   (from (list (hash 'author "Alice" 'article-title "A" 'date "2019-01-01")
;;               (hash 'author "Alice" 'article-title "B" 'date "2019-01-02")
;;               (hash 'author "Bob" 'article-title "C" 'date "2019-01-03"))
;;   (aggregate 'article-title #:using count #:by 'author))
;; =>
;; (list (hash 'author "Alice" 'article-title 2)
;;       (hash 'author "Bob" 'article-title 1))
(define (aggregate col-name #:using aggregator #:by by-col-name) (error 'aggregate "Not implemented"))

;; Aggregator
(define (count . l) (error 'count "Not implemented"))

;; +, max, and min already work as Aggregators.
 
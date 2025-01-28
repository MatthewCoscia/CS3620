#lang racket

(provide query/rows
         query
         from
         where
         select
         join
         limit
         distinct
         aggregate
         count
         order-by
         extend
         join/hash)

(require racket/list
         racket/set)

;; A QueryResult holds a list of row-hashes
(struct query-result (data)
  #:transparent)

;; -----------------------------------------------------------------------------
;; 1) query/rows and query
;; -----------------------------------------------------------------------------

;; query/rows : QueryResult [Clause ...] -> (ListOf Row)
;; Executes a series of clauses on an initial QueryResult, returning a list of rows.
(define (query/rows initial-qresult . clauses)
  ;; Produces a final table (list-of-rows)
  (query-result-data
   (apply query initial-qresult clauses)))

;; query : QueryResult [Clause ...] -> QueryResult
;; Executes a series of clauses on an initial QueryResult, returning a new QueryResult.
(define (query initial-qresult . clauses)
  ;; Applies each clause in order
  (foldl (lambda (clause current-qresult)
           (clause current-qresult))
         initial-qresult
         clauses))

;; -----------------------------------------------------------------------------
;; 2) from
;; -----------------------------------------------------------------------------

;; from : Table [#:qualify Symbol] -> QueryResult
;; Converts a table of rows into a QueryResult. Optionally qualifies column names with a prefix.
(define (from table #:qualify [prefix #f])
  (define qualified-data
    (if prefix
        (for/list ([row table])
          (prefix-keys row prefix))
        table))
  (query-result qualified-data))

;; Helper for prefixing
(define (prefix-keys row prefix)
  (for/hash ([(k v) (in-hash row)])
    (values (string->symbol (format "~a.~a"
                                    (symbol->string prefix)
                                    (symbol->string k)))
            v)))

;; -----------------------------------------------------------------------------
;; 3) where
;; -----------------------------------------------------------------------------

;; where : (Row -> Boolean) -> Clause
;; Filters rows in a QueryResult based on a predicate function.
(define (where keep?)
  (lambda (qr)
    (define data (query-result-data qr))
    (query-result (filter keep? data))))

;; -----------------------------------------------------------------------------
;; 4) select
;; -----------------------------------------------------------------------------

;; select : ColName ... -> Clause
;; Projects specified columns from rows in a QueryResult. Raises an error if columns are missing.
(define (select . col-names)
  (lambda (qr)
    (define data (query-result-data qr))
    (define selected
      (for/list ([row data])
        ;; Check that all requested columns exist
        (for ([c (in-list col-names)])
          (unless (hash-has-key? row c)
            (error 'select (format "Column ~a not present in row: ~v" c row))))
        ;; produce a new row with only those columns:
        (for/hash ([(k v) (in-hash row)]
                   #:when (memq k col-names))
          (values k v))))
    (query-result selected)))

;; -----------------------------------------------------------------------------
;; 5) join
;; -----------------------------------------------------------------------------

;; join : QueryResult ColName ColName -> Clause
;; Performs a nested-loop inner join on two QueryResults, matching rows based on the given columns.
;; Main join function
(define (join qr2 col1 col2)
  (lambda (qr1)
    (define data1 (query-result-data qr1))
    (define data2 (query-result-data qr2))

    ;; Check for column conflicts
    (check-column-conflicts data1 data2)

    ;; Perform the nested-loop join
    (define joined (nested-loop-join data1 data2 col1 col2))

    ;; Return the query result
    (query-result joined)))

;; Helper: Check for column conflicts
(define (check-column-conflicts data1 data2)
  (when (and (pair? data1) (pair? data2))
    (define cols1 (hash-keys (first data1)))
    (define cols2 (hash-keys (first data2)))
    (define conflicts (set-intersect (set cols1) (set cols2)))
    (unless (set-empty? conflicts)
      (error 'join
             (format "Conflicting columns: ~v" (set->list conflicts))))))

;; Helper: Perform nested-loop join
(define (nested-loop-join data1 data2 col1 col2)
  (for*/list ([r1 (in-list data1)]
              [r2 (in-list data2)]
              #:when (equal? (hash-ref r1 col1)
                             (hash-ref r2 col2)))
    (hash-union r1 r2)))

;; Helper: Union two row-hashes
(define (hash-union h1 h2)
  (define new-hash (hash-copy h1))
  (for ([(k v) (in-hash h2)])
    (hash-set! new-hash k v))
  new-hash)


;; -----------------------------------------------------------------------------
;; 6) limit
;; -----------------------------------------------------------------------------

;; limit : Integer -> Clause
;; Retains only the first n rows of a QueryResult.
(define (limit n)
  (lambda (qr)
    (define data (query-result-data qr))
    (query-result (take data n))))

;; -----------------------------------------------------------------------------
;; 7) distinct
;; -----------------------------------------------------------------------------

;; distinct : -> Clause
;; Removes duplicate rows from a QueryResult.
(define (distinct)
  (lambda (qr)
    (define data (query-result-data qr))
    (query-result (remove-duplicates data))))

;; -----------------------------------------------------------------------------
;; 8) aggregate
;; -----------------------------------------------------------------------------

;; aggregate : ColName #:using Aggregator #:by ColName -> Clause
;; Groups rows by one column and applies an aggregator to another column.
;; Main aggregate function
(define (aggregate col-name #:using aggregator #:by by-col)
  (lambda (qr)
    (define data (query-result-data qr))

    ;; Validate the columns
    (validate-columns data col-name by-col)

    ;; Group rows by the `by-col` key
    (define groups (group-by data col-name by-col))

    ;; Aggregate grouped values
    (define aggregated (aggregate-groups groups aggregator col-name by-col))

    ;; Return the aggregated query result
    (query-result aggregated)))

;; Helper: Validate that the required columns exist
(define (validate-columns data col-name by-col)
  (when (and (pair? data))
    (unless (hash-has-key? (first data) col-name)
      (error 'aggregate (format "Missing column ~a" col-name)))
    (unless (hash-has-key? (first data) by-col)
      (error 'aggregate (format "Missing column ~a" by-col)))))

;; Helper: Group rows by the `by-col` key
(define (group-by data col-name by-col)
  (define groups (make-hash))
  (for ([row (in-list data)])
    (define key (hash-ref row by-col))
    (define val (hash-ref row col-name))
    (hash-update! groups key (lambda (old) (cons val old)) '()))
  groups)

;; Helper: Aggregate grouped values
(define (aggregate-groups groups aggregator col-name by-col)
  (for/list ([(gk vals) (in-hash groups)])
    (hash by-col gk
          col-name (apply aggregator vals))))


;; -----------------------------------------------------------------------------
;; 9) count aggregator
;; -----------------------------------------------------------------------------

;; count : Any ... -> Integer
;; Aggregator that counts the number of elements.
(define (count . vals)
  (length vals))

;; -----------------------------------------------------------------------------
;; 3.2 Additional Features
;; -----------------------------------------------------------------------------

;; order-by : ColName #:compare (Any Any -> Boolean) -> Clause
;; Sorts rows in a QueryResult by a specified column using a comparator function.
(define (order-by col #:compare [cmp <])
  (lambda (qr)
    (define data (query-result-data qr))
    (define sorted
      (sort data
            (lambda (r1 r2)
              (cmp (hash-ref r1 col)
                   (hash-ref r2 col))))) 
    (query-result sorted)))

;; extend : ColName (Row -> Any) -> Clause
;; Adds a new column to rows in a QueryResult based on a computed function.
(define (extend new-col-name f)
  (lambda (qr)
    (define data (query-result-data qr))
    (define new-data
      (for/list ([row data])
        (hash-set row new-col-name (f row))))
    (query-result new-data)))


;; -----------------------------------------------------------------------------
;; 3.4 Extra Credit 1
;; -----------------------------------------------------------------------------

;; join/hash : QueryResult ColName ColName -> Clause
;; Performs a hash-based inner join on two QueryResults, matching rows based on the given columns.
;; Main join/hash function
(define (join/hash qr2 col1 col2)
  (lambda (qr1)
    ;; Build hash table for the right dataset
    (define table-hash (build-hash-table-list (query-result-data qr2) col2))

    ;; Process rows in the left dataset using the hash table
    (define left-rows (query-result-data qr1))
    (define joined-rows
      (process-left-rows left-rows col1 table-hash))

    ;; Return the query result
    (query-result joined-rows)))

;; Helper: Build a hash table from a list keyed by a column
(define (build-hash-table-list rows col)
  (define table (make-hash))
  (for ([row (in-list rows)])
    (define key (hash-ref row col 'not-found))
    (hash-update! table key (lambda (old) (cons row old)) '()))
  table)

;; Helper: Process rows in the left dataset
(define (process-left-rows left-rows col1 table-hash)
  (for*/list ([row1 (in-list left-rows)]
              [row2 (in-list (hash-ref table-hash (hash-ref row1 col1 'not-found) '()))])
    (hash-union row1 row2)))



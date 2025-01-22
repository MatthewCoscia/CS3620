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
         racket/stream
         racket/set
         racket/dict)

(struct query-result (data)
  #:transparent)
;; data : (Streamof (Hashof Symbol Any))

;; -----------------------------------------------------------------------------
;; 1) query/rows and query
;; -----------------------------------------------------------------------------

;; query/rows : QueryResult [Clause ...] -> (ListOf Row)
;; Executes a series of clauses on an initial QueryResult, returning a list of rows.
(define (query/rows initial-qresult . clauses)
  (stream->list
   (query-result-data
    (apply query initial-qresult clauses))))

;; query : QueryResult [Clause ...] -> QueryResult
;; Executes a series of clauses on an initial QueryResult, returning a new QueryResult.
(define (query initial-qresult . clauses)
  (foldl (lambda (clause current-qresult)
           (clause current-qresult))
         initial-qresult
         clauses))

;; -----------------------------------------------------------------------------
;; 2) from
;; -----------------------------------------------------------------------------

;; from : Table [#:qualify Symbol] -> QueryResult
;; Converts a table of rows into a QueryResult as a stream. Optionally qualifies column names with a prefix.
(define (from table #:qualify [prefix #f])
  (define s
    (if prefix
        (for/stream ([row table])
          (prefix-keys row prefix))
        (for/stream ([row table])
          row)))
  (query-result s))

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
;; Filters rows in a QueryResult (stream) based on a predicate function.
(define (where keep?)
  (lambda (qr)
    (query-result
     (stream-filter keep?
                    (query-result-data qr)))))

;; -----------------------------------------------------------------------------
;; 4) select
;; -----------------------------------------------------------------------------

;; select : ColName ... -> Clause
;; Projects specified columns from rows in a QueryResult. Raises an error if columns are missing.
(define (select . col-names)
  (lambda (qr)
    (query-result
     (stream-map
      (lambda (row)
        ;; Check if all requested columns exist
        (for ([c (in-list col-names)])
          (unless (hash-has-key? row c)
            (error 'select
                   (format "Missing column ~a in row: ~v" c row))))
        ;; Keep only these columns
        (for/hash ([(k v) (in-hash row)]
                   #:when (memq k col-names))
          (values k v)))
      (query-result-data qr)))))

;; -----------------------------------------------------------------------------
;; Helpers: hash-union, list->stream
;; -----------------------------------------------------------------------------

(define (hash-union h1 h2)
  (define new-hash (hash-copy h1))
  (for ([(k v) (in-hash h2)])
    (hash-set! new-hash k v))
  new-hash)

(define (list->stream lst)
  (for/stream ([x (in-list lst)])
    x))

;; -----------------------------------------------------------------------------
;; 5) join
;; -----------------------------------------------------------------------------

;; join : QueryResult ColName ColName -> Clause
;; Performs a nested-loop inner join on two QueryResults, matching rows based on the given columns.
;; Main Join Function
(define (join qr2 col1 col2)
  (lambda (qr1)
    (define s1 (query-result-data qr1))
    (define s2 (query-to-list qr2)) ; Force entire right side into memory

    ;; Check column conflicts
    (when (and (not (stream-empty? s1)) (not (null? s2)))
      (check-column-conflicts s1 s2))

    ;; Perform the nested-loop join
    (define final-stream (nested-loop-join s1 s2 col1 col2))

    ;; Return the query result without limiting rows
    (query-result final-stream)))

;; Helper: Convert query-result to list
(define (query-to-list qr)
  (stream->list (query-result-data qr)))

;; Helper: Check column conflicts
(define (check-column-conflicts s1 s2)
  (define cols1 (hash-keys (stream-first s1)))
  (define cols2 (hash-keys (first s2)))
  (define conflicts (set-intersect (set cols1) (set cols2)))
  (when (not (set-empty? conflicts))
    (error 'join (format "Conflicting columns: ~v" (set->list conflicts)))))

;; Helper: Perform the join operation on two rows
(define (join-rows row1 col1 s2 col2)
  (define val1 (hash-ref row1 col1))
  (for/list ([row2 (in-list s2)]
             #:when (and (hash? row2) ; Ensure `row2` is a hash
                         (equal? val1 (hash-ref row2 col2 'not-found))))
    (hash-union row1 row2)))

;; Custom stream-flat-map implementation
(define (stream-flat-map f s)
  (if (stream-empty? s)
      empty-stream
      (stream-append (f (stream-first s))
                     (stream-flat-map f (stream-rest s)))))

;; Custom implementation of stream->stream
(define (stream->stream f s)
  (if (stream-empty? s)
      empty-stream
      (stream-cons (f (stream-first s))
                   (stream->stream f (stream-rest s)))))


;; Helper: Perform the nested-loop join
(define (nested-loop-join s1 s2 col1 col2)
  (define (process-row row1)
    (join-rows row1 col1 s2 col2)) ; Returns a list
  (stream-flat-map
   (lambda (row1)
     (list->stream (process-row row1))) ; Convert the list to a stream
   s1))




;; -----------------------------------------------------------------------------
;; 6) limit
;; -----------------------------------------------------------------------------

;; limit : Integer -> Clause
;; Retains only the first n rows of a QueryResult (stream).
(define (limit n)
  (lambda (qr)
    (query-result
     (stream-take (query-result-data qr)
                  n))))

;; -----------------------------------------------------------------------------
;; 7) distinct
;; -----------------------------------------------------------------------------

;; distinct : -> Clause
;; Removes duplicate rows from a QueryResult (stream).
;; Main distinct function
(define (distinct)
  (lambda (qr)
    (define s (query-result-data qr))
    (query-result (filter-distinct s))))

;; Helper: Filter distinct rows in a stream
(define (filter-distinct s)
  (define seen (make-hash))
  (distinct-helper s seen))

;; Helper: Recursively process the stream to keep distinct rows
(define (distinct-helper st seen)
  (stream-lazy
   (cond
     [(stream-empty? st)
      (list->stream '())] ; produce an empty stream
     [else
      (process-row (stream-first st)
                   (stream-rest st)
                   seen)])))

;; Helper: Process a single row, adding it if it's distinct
(define (process-row row rest-stream seen)
  (if (hash-has-key? seen row)
      (distinct-helper rest-stream seen) ; Skip duplicate
      (begin
        (hash-set! seen row #t) ; Mark row as seen
        (stream-cons row
                     (distinct-helper rest-stream seen))))) ; Include the row


;; -----------------------------------------------------------------------------
;; 8) aggregate
;; -----------------------------------------------------------------------------

;; aggregate : ColName #:using Aggregator #:by ColName -> Clause
;; Groups rows by one column and applies an aggregator to another column.
;; Main aggregate function
(define (aggregate col-name #:using aggregator #:by by-col)
  (lambda (qr)
    (define s (query-result-data qr))
    (define row-list (stream->list s))

    ;; Validate the columns
    (validate-columns row-list col-name by-col)

    ;; Group rows by `by-col`
    (define groups (group-by-column row-list col-name by-col))

    ;; Aggregate grouped values
    (define aggregated (aggregate-groups groups aggregator col-name by-col))

    ;; Return the aggregated query result
    (query-result (list->stream aggregated))))

;; Helper: Validate that the required columns exist
(define (validate-columns row-list col-name by-col)
  (when (and (pair? row-list))
    (unless (hash-has-key? (first row-list) col-name)
      (error 'aggregate (format "Missing column ~a" col-name)))
    (unless (hash-has-key? (first row-list) by-col)
      (error 'aggregate (format "Missing column ~a" by-col)))))

;; Helper: Group rows by a specific column
(define (group-by-column row-list col-name by-col)
  (define groups (make-hash))
  (for ([row (in-list row-list)])
    (define key (hash-ref row by-col))
    (define val (hash-ref row col-name))
    (hash-update! groups key (lambda (old) (cons val old)) '()))
  groups)

;; Helper: Aggregate grouped values
(define (aggregate-groups groups aggregator col-name by-col)
  (for/list ([(group-key values) (in-hash groups)])
    (hash by-col group-key
          col-name (apply aggregator values))))


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
;; Converts the stream to a list during sorting.
(define (order-by col #:compare [cmp <])
  (lambda (qr)
    (define row-list (stream->list (query-result-data qr)))
    (define sorted-list
      (sort row-list
            (lambda (r1 r2)
              (cmp (hash-ref r1 col)
                   (hash-ref r2 col)))))
    (query-result (list->stream sorted-list))))

;; extend : ColName (Row -> Any) -> Clause
;; Adds a new column to rows in a QueryResult based on a computed function.
(define (extend new-col-name f)
  (lambda (qr)
    (define s (query-result-data qr))
    (define extended-stream
      (stream-map
       (lambda (row)
         (hash-set row new-col-name (f row)))
       s))
    (query-result extended-stream)))



;; -----------------------------------------------------------------------------
;; 3.4 Extra Credit 1
;; -----------------------------------------------------------------------------

;; join/hash : QueryResult ColName ColName -> Clause
;; Performs a hash-based inner join on two QueryResults, matching rows based on the given columns.
;; Main join/hash function
(define (join/hash qr2 col1 col2)
  (lambda (qr1)
    ;; Build hash table for the right dataset
    (define table-hash (build-hash-table-hash-join (query-result-data qr2) col2))

    ;; Stream the left dataset and process rows
    (define left-rows (query-result-data qr1))
    (define final-stream
      (stream-flat-map
       (lambda (row1)
         (list->stream (process-hash-join-row row1 col1 table-hash)))
       left-rows))

    ;; Return query result
    (query-result final-stream)))

;; Helper: Build a hash table from a stream keyed by a column (lazily)
(define (build-hash-table-hash-join stream col)
  (define table (make-hash))
  (for ([row (in-stream stream)]) ; Process stream lazily
    (define key (hash-ref row col 'not-found))
    (hash-update! table key (lambda (old) (cons row old)) '()))
  table)

;; Helper: Process a single row for hash join
(define (process-hash-join-row row1 col1 table-hash)
  (define key (hash-ref row1 col1 'not-found))
  (define matches (hash-ref table-hash key '()))
  (for/list ([row2 (in-list matches)])
    (hash-union row1 row2)))


;; -----------------------------------------------------------------------------
;; TESTS
;; -----------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; Helper from assignment instructions:
  ;; Produces a stream with the given rows,
  ;; then raises an error if forced beyond them.
  (define (rows-then-error rows)
    (stream-append
     (for/stream ([row rows]) row)
     (stream-lazy
       (error 'rows-then-error "stream forced too far"))))

  ;; Helper to create a QueryResult from a list of hashes
  (define (make-qr-from-list lst)
    (query-result (for/stream ([r lst]) r)))

  ;; ----------------------------------------------------------------------------
  ;; Test 1: limit terminates the stream early
  ;; ----------------------------------------------------------------------------
  ;; Ensures `limit` stops processing after retrieving the first `n` rows.
  (test-case "limit terminates the stream early"
    (define dummy-3
      (rows-then-error
       (list (hash 'id 1)
             (hash 'id 2)
             (hash 'id 3))))
    (define result
      (query/rows
        (query-result dummy-3)
        (limit 2))) ; Should retrieve only 2 rows
    (check-equal? (length result) 2))

  ;; ----------------------------------------------------------------------------
  ;; Test 2: where + limit also terminates early
  ;; ----------------------------------------------------------------------------
  ;; Ensures `where` filters rows before `limit` stops processing.
  (test-case "where + limit terminates early"
    (define dummy-5
      (rows-then-error
       (list (hash 'val 1)
             (hash 'val 2)
             (hash 'val 3)
             (hash 'val 4)
             (hash 'val 5))))
    (define result
      (query/rows
        (query-result dummy-5)
        (where (lambda (row) (< (hash-ref row 'val) 4))) ; Only first 3 rows pass
        (limit 2))) ; Should stop after 2 rows
    (check-equal? (length result) 2))


  ;; ----------------------------------------------------------------------------
  ;; Test 3: order-by sorts the rows
  ;; ----------------------------------------------------------------------------
  ;; Ensures `order-by` correctly sorts rows by a specified column.
  (test-case "order-by sorts rows by a column"
    (define dummy-rows
      (make-qr-from-list
       (list (hash 'id 3 'name "Charlie")
             (hash 'id 1 'name "Alice")
             (hash 'id 2 'name "Bob"))))
    (define result
      (query/rows
        dummy-rows
        (order-by 'id #:compare <))) ; Sort by `id` in ascending order
    (check-equal? result
                  (list (hash 'id 1 'name "Alice")
                        (hash 'id 2 'name "Bob")
                        (hash 'id 3 'name "Charlie"))))

  ;; ----------------------------------------------------------------------------
  ;; Test 4: extend adds new computed columns
  ;; ----------------------------------------------------------------------------
  ;; Ensures `extend` adds a computed column to each row.
  (test-case "extend adds new computed columns"
    (define dummy-rows
      (make-qr-from-list
       (list (hash 'x 1)
             (hash 'x 2)
             (hash 'x 3))))
    (define result
      (query/rows
        dummy-rows
        (extend 'square (lambda (row) (sqr (hash-ref row 'x)))))) ; Add `square`
    (check-equal? result
                  (list (hash 'x 1 'square 1)
                        (hash 'x 2 'square 4)
                        (hash 'x 3 'square 9))))

  ;; ----------------------------------------------------------------------------
  ;; Test 5: distinct removes duplicate rows
  ;; ----------------------------------------------------------------------------
  ;; Ensures `distinct` removes duplicate rows from the dataset.
  (test-case "distinct removes duplicate rows"
    (define dummy-rows
      (make-qr-from-list
       (list (hash 'id 1)
             (hash 'id 1)
             (hash 'id 2)
             (hash 'id 3)
             (hash 'id 3))))
    (define result
      (query/rows
        dummy-rows
        (distinct))) ; Remove duplicates
    (check-equal? result
                  (list (hash 'id 1)
                        (hash 'id 2)
                        (hash 'id 3))))

  ;; ----------------------------------------------------------------------------
  ;; Test 6: complex query combining all clauses
  ;; ----------------------------------------------------------------------------
  ;; Ensures a complex query combining multiple clauses produces correct results.
  (test-case "complex query combining multiple clauses"
    (define dummy-rows
      (make-qr-from-list
       (list (hash 'group "A" 'val 1)
             (hash 'group "A" 'val 2)
             (hash 'group "B" 'val 3)
             (hash 'group "B" 'val 4)
             (hash 'group "C" 'val 5))))
    (define result
      (query/rows
        dummy-rows
        (aggregate 'val #:using + #:by 'group) ; Sum values by group
        (where (lambda (row) (>= (hash-ref row 'val) 5))) ; Filter for val >= 5
        (order-by 'val #:compare >) ; Sort descending
        (limit 2))) ; Take only 2 rows
    (check-equal? result
                  (list (hash 'group "B" 'val 7)
                        (hash 'group "C" 'val 5)))))






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
         extend)

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

(define (query/rows initial-qresult . clauses)
  (stream->list
   (query-result-data
    (apply query initial-qresult clauses))))

(define (query initial-qresult . clauses)
  (foldl (lambda (clause current-qresult)
           (clause current-qresult))
         initial-qresult
         clauses))

;; -----------------------------------------------------------------------------
;; 2) from
;; -----------------------------------------------------------------------------

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

(define (where keep?)
  (lambda (qr)
    (query-result
     (stream-filter keep?
                    (query-result-data qr)))))

;; -----------------------------------------------------------------------------
;; 4) select
;; -----------------------------------------------------------------------------
;; Use #:when in for/hash so that we never produce 0 or 1 value
;; when the column doesn't match.

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




;; -----------------------------------------------------------------------------
;; 6) limit
;; -----------------------------------------------------------------------------

(define (limit n)
  (lambda (qr)
    (query-result
     (stream-take (query-result-data qr)
                  n))))

;; -----------------------------------------------------------------------------
;; 7) distinct
;; -----------------------------------------------------------------------------

(define (distinct)
  (lambda (qr)
    (define s (query-result-data qr))
    (define seen (make-hash))

    (define (distinct-helper st)
      (stream-lazy
       (cond
         [(stream-empty? st)
          (list->stream '())] ; produce empty stream
         [else
          (define r (stream-first st))
          (if (hash-has-key? seen r)
              (distinct-helper (stream-rest st))
              (begin
                (hash-set! seen r #t)
                (stream-cons r
                             (distinct-helper (stream-rest st)))))])))

    (query-result (distinct-helper s))))

;; -----------------------------------------------------------------------------
;; 8) aggregate
;; -----------------------------------------------------------------------------

(define (aggregate col-name #:using aggregator #:by by-col)
  (lambda (qr)
    (define s (query-result-data qr))
    (define row-list (stream->list s))
    (when (and (pair? row-list))
      (unless (hash-has-key? (first row-list) col-name)
        (error 'aggregate (format "Missing column ~a" col-name)))
      (unless (hash-has-key? (first row-list) by-col)
        (error 'aggregate (format "Missing column ~a" by-col))))

    ;; Group rows by `by-col`
    (define groups (make-hash))
    (for ([r (in-list row-list)])
      (define key (hash-ref r by-col))
      (define val (hash-ref r col-name))
      (hash-update! groups key (lambda (old) (cons val old)) '()))

    ;; Build final aggregated rows: two columns
    (define aggregated
      (for/list ([(gk vals) (in-hash groups)])
        (hash by-col  gk
              col-name (apply aggregator vals))))

    (query-result (list->stream aggregated))))

;; -----------------------------------------------------------------------------
;; 9) count aggregator
;; -----------------------------------------------------------------------------

(define (count . vals)
  (length vals))

;; -----------------------------------------------------------------------------
;; 3.2 Additional Features
;; -----------------------------------------------------------------------------

(define (order-by col #:compare [cmp <])
  (lambda (qr)
    (define row-list (stream->list (query-result-data qr)))
    (define sorted-list
      (sort row-list
            (lambda (r1 r2)
              (cmp (hash-ref r1 col)
                   (hash-ref r2 col)))))
    (query-result (list->stream sorted-list))))

(define (extend new-col-name f)
  (lambda (qr)
    (define s (query-result-data qr))
    (define extended-stream
      (stream-map
       (lambda (row)
         (hash-set row new-col-name (f row)))
       s))
    (query-result extended-stream)))

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
  ;; Test 1: limit short-circuits the stream
  ;; ----------------------------------------------------------------------------
  (test-case "limit short-circuits the stream"
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
  ;; Test 2: where + limit also short-circuits
  ;; ----------------------------------------------------------------------------
  (test-case "where + limit short-circuits"
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






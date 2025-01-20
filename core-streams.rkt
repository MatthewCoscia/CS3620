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
         join/hash
         aggregate/multi)

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

(define (join qr2 col1 col2)
  (lambda (qr1)
    (define s1 (query-result-data qr1))
    (define s2 (stream->list (query-result-data qr2))) ; force entire right side

    (define first1 (stream-first s1 (void)))
    (define rest1  (stream-rest s1))

    ;; If both sides have data, check for column conflicts
    (when (and (not (void? first1))
               (not (null? s2)))
      (define cols1 (hash-keys first1))
      (define cols2 (hash-keys (first s2)))
      (define conflicts (set-intersect (set cols1) (set cols2)))
      (unless (set-empty? conflicts)
        (error 'join
               (format "Conflicting columns: ~v"
                       (set->list conflicts)))))


    ;; Nested-loop join
    (define (join-rows row1)
      (define val1 (hash-ref row1 col1))
      (for/list ([row2 s2]
                 #:when (equal? val1 (hash-ref row2 col2)))
        (hash-union row1 row2)))

    (define final-stream
      (stream-append
       (if (void? first1)
           (list->stream '())        ; empty stream if s1 is empty
           (list->stream (join-rows first1)))
       (for/stream ([r (in-stream rest1)])
         (for*/stream ([r2 (in-list (join-rows r))])
           r2))))

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

;; -----------------------------------------------------------------------------
;; Extra Credit: 3.4 - join/hash (stream-based)
;; -----------------------------------------------------------------------------
;; Same signature as `join`, but uses a hash join approach.
;; Forces the entire "right side" of the stream (qr2) to build the hash table,
;; then reads "left side" (qr1).
(define (join/hash qr2 col1 col2)
  (lambda (qr1)
    ;; Force the entire right side into a list
    (define right-rows (stream->list (query-result-data qr2)))

    ;; Build a hash table keyed by col2 -> list-of-rows
    (define table-hash (make-hash))
    (for ([row right-rows])
      (define key (hash-ref row col2))
      (hash-update! table-hash key (lambda (old) (cons row old)) '()))

    ;; Force the left side as well, since we need to iterate fully
    (define left-rows (stream->list (query-result-data qr1)))

    ;; Check conflicts if both sides have data
    (when (and (pair? left-rows) (pair? right-rows))
      (define cols1 (hash-keys (first left-rows)))
      (define cols2 (hash-keys (first right-rows)))
      (define conflicts (set-intersect (set cols1) (set cols2)))
      (unless (set-empty? conflicts)
        (error 'join/hash
               (format "Conflicting columns: ~v" (set->list conflicts)))))

    ;; Perform the hash join
    (define joined
      (for*/list ([r1 left-rows]
                  [r2 (in-list (hash-ref table-hash (hash-ref r1 col1) '()))])
        (hash-union r1 r2)))

    ;; Convert back to a stream
    (query-result (list->stream joined))))

;; -----------------------------------------------------------------------------
;; Extra Credit: 3.5 - aggregate/multi (stream-based)
;; -----------------------------------------------------------------------------
;; aggregate/multi : (Listof (ColName #:using Aggregator))
;;                   #:by (Listof ColName)
;;                   -> Clause
;; Groups by multiple columns and applies multiple aggregator columns simultaneously.
(define (aggregate/multi col-agg-list #:by group-cols)
  (lambda (qr)
    ;; Force the entire stream, since grouping requires seeing all rows
    (define row-list (stream->list (query-result-data qr)))

    ;; Check group columns
    (unless (null? row-list)
      (for ([gc (in-list group-cols)])
        (unless (hash-has-key? (first row-list) gc)
          (error 'aggregate/multi (format "Missing group column ~a" gc))))
      ;; Check aggregator columns
      (for ([spec (in-list col-agg-list)])
        (define col-name (first spec))
        (unless (hash-has-key? (first row-list) col-name)
          (error 'aggregate/multi
                 (format "Missing aggregator column ~a" col-name)))))


    ;; Group rows by (list of group-col values)
    (define groups (make-hash))
    (for ([row (in-list row-list)])
      (define key (map (lambda (gc) (hash-ref row gc)) group-cols))
      (hash-update! groups key (lambda (old) (cons row old)) '()))

    ;; For each group, apply each aggregator to its column
    (define aggregated
      (for/list ([(group-key rows-in-group) (in-hash groups)])
        ;; group-key is the list of grouping column values
        ;;  => produce (gc => value) pairs
        (define group-pairs
          (for/list ([gc (in-list group-cols)]
                     [val (in-list group-key)])
            (values gc val)))

        ;; aggregator columns
        (define aggregator-pairs
          (for/list ([spec (in-list col-agg-list)])
            (define col-name    (first spec))
            (define aggregator  (second spec)) ; e.g. #:using aggregator
            (define col-values  (map (lambda (r) (hash-ref r col-name))
                                     rows-in-group))
            (values col-name (apply aggregator col-values))))

        ;; Combine group-pairs + aggregator-pairs into one row
        (hash (append group-pairs aggregator-pairs))))

    ;; Convert final list of row-hashes back to a stream
    (query-result (list->stream aggregated))))



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

  ;; A small tool to convert a list of (Hash) to a QueryResult
  (define (make-qr-from-list lst)
    (query-result (for/stream ([r lst]) r)))

  ;; ----------------------------------------------------------------------------
  ;; Test 1: limit does not force beyond N rows
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
    (check-equal? (length result) 2)
    ;; If code tried to read the 3rd row, we'd see "stream forced too far."
    )

  ;; ----------------------------------------------------------------------------
  ;; Test 2: where + limit also should not force everything
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
        (where (lambda (row) (< (hash-ref row 'val) 10))) ; all pass
        (limit 2)))
    (check-equal? (length result) 2)
    ;; We never read beyond row #2.
    ))





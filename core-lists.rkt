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
         racket/set)

;; A QueryResult holds a list of row-hashes
(struct query-result (data)
  #:transparent)

;; -----------------------------------------------------------------------------
;; 1) query/rows and query
;; -----------------------------------------------------------------------------

(define (query/rows initial-qresult . clauses)
  ;; Produces a final table (list-of-rows)
  (query-result-data
   (apply query initial-qresult clauses)))

(define (query initial-qresult . clauses)
  ;; Applies each clause in order
  (foldl (lambda (clause current-qresult)
           (clause current-qresult))
         initial-qresult
         clauses))

;; -----------------------------------------------------------------------------
;; 2) from
;; -----------------------------------------------------------------------------

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

(define (where keep?)
  (lambda (qr)
    (define data (query-result-data qr))
    (query-result (filter keep? data))))

;; -----------------------------------------------------------------------------
;; 4) select
;; -----------------------------------------------------------------------------
;; The key fix is using #:when to skip unwanted columns,
;; rather than using (when ...) which yields (void) on false.

(define (select . col-names)
  (lambda (qr)
    (define data (query-result-data qr))
    (define selected
      (for/list ([row data])
        ;; Check that all requested columns exist
        (for ([c (in-list col-names)])
          (unless (hash-has-key? row c)
            (error 'select (format "Column ~a not present in row: ~v" c row))))
        ;; Now produce a new row with only those columns:
        (for/hash ([(k v) (in-hash row)]
                   #:when (memq k col-names))
          (values k v))))
    (query-result selected)))

;; -----------------------------------------------------------------------------
;; 5) join
;; -----------------------------------------------------------------------------

(define (join qr2 col1 col2)
  (lambda (qr1)
    (define data1 (query-result-data qr1))
    (define data2 (query-result-data qr2))

    ;; Check conflicts if both sides are non-empty
    (when (and (pair? data1)
               (pair? data2))
      (define cols1 (hash-keys (first data1)))
      (define cols2 (hash-keys (first data2)))
      (define conflicts (set-intersect (set cols1) (set cols2)))
      (unless (set-empty? conflicts)
        (error 'join (format "Conflicting columns: ~v"
                             (set->list conflicts)))))

    ;; Nested-loop join
    (define joined
      (for*/list ([r1 (in-list data1)]
                  [r2 (in-list data2)]
                  #:when (equal? (hash-ref r1 col1)
                                 (hash-ref r2 col2)))
        (hash-union r1 r2)))

    (query-result joined)))

;; Helper to union two row-hashes
(define (hash-union h1 h2)
  (define new-hash (hash-copy h1))
  (for ([(k v) (in-hash h2)])
    (hash-set! new-hash k v))
  new-hash)

;; -----------------------------------------------------------------------------
;; 6) limit
;; -----------------------------------------------------------------------------

(define (limit n)
  (lambda (qr)
    (define data (query-result-data qr))
    (query-result (take data n))))

;; -----------------------------------------------------------------------------
;; 7) distinct
;; -----------------------------------------------------------------------------

(define (distinct)
  (lambda (qr)
    (define data (query-result-data qr))
    (query-result (remove-duplicates data))))

;; -----------------------------------------------------------------------------
;; 8) aggregate
;; -----------------------------------------------------------------------------
;; Key fix: produce exactly 2 columns in the row-hash, no (values ...).

(define (aggregate col-name #:using aggregator #:by by-col)
  (lambda (qr)
    (define data (query-result-data qr))
    (when (and (pair? data))
      (unless (hash-has-key? (first data) col-name)
        (error 'aggregate (format "Missing column ~a" col-name)))
      (unless (hash-has-key? (first data) by-col)
        (error 'aggregate (format "Missing column ~a" by-col))))

    (define groups (make-hash))
    (for ([row (in-list data)])
      (define key (hash-ref row by-col))
      (define val (hash-ref row col-name))
      (hash-update! groups key (lambda (old) (cons val old)) '()))

    (define aggregated
      (for/list ([(gk vals) (in-hash groups)])
        (hash by-col  gk
              col-name (apply aggregator vals))))
    (query-result aggregated)))

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
    (define data (query-result-data qr))
    (define sorted
      (sort data
            (lambda (r1 r2)
              (cmp (hash-ref r1 col)
                   (hash-ref r2 col))))) 
    (query-result sorted)))

(define (extend new-col-name f)
  (lambda (qr)
    (define data (query-result-data qr))
    (define new-data
      (for/list ([row data])
        (hash-set row new-col-name (f row))))
    (query-result new-data)))

;; -----------------------------------------------------------------------------
;; Extra Credit: 3.4 - join/hash
;; -----------------------------------------------------------------------------
(define (join/hash qr2 col1 col2)
  (lambda (qr1)
    (define data1 (query-result-data qr1))
    (define data2 (query-result-data qr2))
    ;; Check conflicts if both sides non-empty
    (when (and (pair? data1) (pair? data2))
      (define cols1 (hash-keys (first data1)))
      (define cols2 (hash-keys (first data2)))
      (define conflicts (set-intersect (set cols1) (set cols2)))
      (unless (set-empty? conflicts)
        (error 'join/hash (format "Conflicting columns: ~v"
                                  (set->list conflicts)))))

    ;; Build a hash table keyed by col2 -> list-of-rows
    (define table-hash (make-hash))
    (for ([row (in-list data2)])
      (define key (hash-ref row col2))
      (hash-update! table-hash key (λ (old) (cons row old)) '()))

    ;; For each row in data1, look up matching rows in table-hash
    (define joined
      (for*/list ([r1 (in-list data1)]
                  [r2 (in-list (hash-ref table-hash (hash-ref r1 col1) '()))])
        (hash-union r1 r2)))

    (query-result joined)))


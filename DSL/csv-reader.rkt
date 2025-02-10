#lang racket

(require racket/string)

;; Define string-blank? if it's not available.
(define (string-blank? s)
  (string=? (string-trim s) ""))

;; Define a structure that holds both the header and the rows of data.
(struct stock-data (header rows)
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSV PARSING HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a header line, detect the delimiter (either tab or comma).
(define (detect-delimiter header-line)
  (cond
    [(regexp-match #px"\t" header-line) "\t"]
    [(regexp-match #px"," header-line) ","]
    [else (error "Could not detect delimiter in header line")]))

;; Load and parse a CSV file with chronological sorting
(define (load-stock-data filename)
  (define data-str (file->string filename))
  (define all-lines (string-split data-str "\n"))
  (define lines (filter (lambda (line) (not (string-blank? line))) all-lines))
  (unless (and (pair? lines) (> (length lines) 1))
    (error "CSV file has insufficient data"))
  
  ;; Process the header line
  (define header-line (car lines))
  (define delimiter (detect-delimiter header-line))
  (define header (map string-trim (string-split header-line delimiter)))
  
  ;; Create a hash mapping header names to their column indexes
  (define header-map (make-hash))
  (for ([i (in-naturals)]
        [field (in-list header)])
    (hash-set! header-map field i))
  
  ;; Validate required headers
  (define required-headers '("Date" "Open" "High" "Low" "Close" "Volume"))
  (for-each
   (lambda (hdr)
     (unless (hash-has-key? header-map hdr)
       (error 'load-stock-data (format "Missing required header: ~a" hdr))))
   required-headers)
  
  ;; Convert a row into a data point
  (define (row->data-point row)
    (define fields (map string-trim (string-split row delimiter)))
    (define date (list-ref fields (hash-ref header-map "Date")))
    (define open (string->number (list-ref fields (hash-ref header-map "Open"))))
    (define high (string->number (list-ref fields (hash-ref header-map "High"))))
    (define low (string->number (list-ref fields (hash-ref header-map "Low"))))
    (define close (string->number (list-ref fields (hash-ref header-map "Close"))))
    (define volume (string->number (list-ref fields (hash-ref header-map "Volume"))))
    (list date open high low close volume))
  
  ;; Parse and sort data by date (oldest first)
  (define unsorted-rows (map row->data-point (cdr lines)))
  (define sorted-rows
    (sort unsorted-rows
          (λ (a b)
            (string<? (car a) (car b)))))  ;; Compare date strings directly
  
  (stock-data header sorted-rows))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example Usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  ;; Load and display sorted data
  (define aapl (load-stock-data "aapl.csv"))
  
  (displayln "Headers:")
  (for-each displayln (stock-data-header aapl))
  
  (displayln "\nFirst 3 sorted rows:")
  (for ([row (take (stock-data-rows aapl) 3)])
    (displayln row)))

(provide load-stock-data
         stock-data
         stock-data-header
         stock-data-rows)
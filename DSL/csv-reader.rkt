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

;; Load and parse a CSV file.
;;
;; Returns a stock-data structure with:
;;  - header: a list of the field names (strings)
;;  - rows: a list of data points (each represented as a list of values)
(define (load-stock-data filename)
  (define data-str (file->string filename))
  (define all-lines (string-split data-str "\n"))
  (define lines (filter (lambda (line) (not (string-blank? line))) all-lines))
  (unless (and (pair? lines) (> (length lines) 1))
    (error "CSV file has insufficient data"))
  
  ;; Process the header line.
  (define header-line (car lines))
  (define delimiter (detect-delimiter header-line))
  (define header (map string-trim (string-split header-line delimiter)))
  
  ;; Display the headers (optional)
  (displayln "CSV Headers:")
  (for-each displayln header)
  
  ;; Create a hash mapping header names to their column indexes.
  (define header-map (make-hash))
  (for ([i (in-naturals)]
        [field (in-list header)])
    (hash-set! header-map field i))
  
  ;; Define the required headers.
  (define required-headers '("Date" "Open" "High" "Low" "Close" "Volume" "Adj Close"))
  
  ;; Check if each required header is present.
  (for-each
   (lambda (hdr)
     (unless (hash-has-key? header-map hdr)
       (error 'load-stock-data (format "Missing required header: ~a" hdr))))
   required-headers)
  
  ;; Check if the CSV includes a "Time" column for intraday data.
  (define has-time? (hash-has-key? header-map "Time"))
  (define parse-timestamp
    (if has-time?
        ;; For intraday data: combine Date and Time.
        (lambda (fields)
          (string-append (list-ref fields (hash-ref header-map "Date"))
                         " " (list-ref fields (hash-ref header-map "Time"))))
        ;; Otherwise, use just the Date.
        (lambda (fields)
          (list-ref fields (hash-ref header-map "Date")))))
  
  ;; Convert a row (string) into a data point (a list of values).
  (define (row->data-point row)
    (define fields (map string-trim (string-split row delimiter)))
    (define timestamp (parse-timestamp fields))
    (define open      (string->number (list-ref fields (hash-ref header-map "Open"))))
    (define high      (string->number (list-ref fields (hash-ref header-map "High"))))
    (define low       (string->number (list-ref fields (hash-ref header-map "Low"))))
    (define close     (string->number (list-ref fields (hash-ref header-map "Close"))))
    (define volume    (string->number (list-ref fields (hash-ref header-map "Volume"))))
    (define adj-close (string->number (list-ref fields (hash-ref header-map "Adj Close"))))
    (list timestamp open high low close volume adj-close))
  
  ;; Parse the remaining data rows.
  (define rows (map row->data-point (cdr lines)))
  (stock-data header rows))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example usage:
;;
;; (define aapl-data (load-stock-data "aapl.csv"))
;;
;; The above call will now error out with a clear message if any required
;; header is missing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (define aapl-data (load-stock-data "aapl.csv"))
  
  ;; Display the parsed headers.
  (displayln "\nParsed Headers:")
  (for-each displayln (stock-data-header aapl-data))
  
  ;; Display the first few data points.
  (displayln "\nFirst few data points:")
  (for-each displayln (take (stock-data-rows aapl-data) 3)))

(provide load-stock-data
         stock-data
         stock-data-header
         stock-data-rows)

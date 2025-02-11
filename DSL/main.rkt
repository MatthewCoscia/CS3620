#lang racket

(require plot
         racket/string
         racket/date
         syntax/parse/define
         racket/contract
         "csv-reader.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Plot Components (with Contracts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (seconds->plot-day s)
  (-> number? number?)
  (/ s 86400))

(define/contract (plot-day->seconds d)
  (-> number? number?)
  (* d 86400))

(define/contract (date->string d)
  (-> date? string?)
  (format "~a-~a-~a" (date-year d) (date-month d) (date-day d)))

;; For our purposes we do not have a precise ticks-descriptor predicate,
;; so we use any/c. (Alternatively, one might define a predicate that checks
;; that the returned value “comes from” a call to ticks.)
(define/contract (date-ticks)
  (-> any/c)
  (ticks 
   ;; Generator lambda:
   (λ (ax-min ax-max)
     (displayln (format "Generator called with ax-min: ~a, ax-max: ~a" ax-min ax-max))
     (define start (plot-day->seconds ax-min))
     (define end   (plot-day->seconds ax-max))
     (displayln (format "Computed start (seconds): ~a, end (seconds): ~a" start end))
     (define range-days (- ax-max ax-min))
     ;; Decide the step size (in days) based on the overall range:
     (define step-days
       (cond
         [(> range-days (* 365 3)) 365]
         [(> range-days 365) 30]
         [else 7]))
     (define step (* step-days 86400))
     (for/list ([d (in-range start end step)])
       (let ([pt (pre-tick (seconds->plot-day d) #t)])
         (displayln (format "Generator produced pre-tick: ~a" pt))
         pt)))
   ;; Formatter lambda:
   (λ (min-tick max-tick pre-ticks)
     (define overall-range (- max-tick min-tick))  ; in days
     (define use-year-only? (> overall-range (* 365 3)))
     (displayln (format "Formatter called with min-tick: ~a, max-tick: ~a, overall-range: ~a, pre-ticks: ~a"
                        min-tick max-tick overall-range pre-ticks))
     (map (λ (v)
            (if (pre-tick? v)
                (let* ([tick-val (pre-tick-value v)]
                       [s (plot-day->seconds tick-val)]
                       [d (seconds->date s #t)]
                       [label (if use-year-only?
                                  (format "~a" (date-year d))
                                  (date->string d))])
                  (displayln (format "Formatter: tick label ~a" label))
                  label)
                (let* ([s (plot-day->seconds v)]
                       [d (seconds->date s #t)]
                       [label (if use-year-only?
                                  (format "~a" (date-year d))
                                  (date->string d))])
                  (displayln (format "Formatter: tick label ~a" label))
                  label)))
          pre-ticks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date Parsing and Dataset Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (parse-date date-str)
  (-> string? number?)
  (define parts (string-split date-str "-"))
  (define year (string->number (first parts)))
  (define month (string->number (second parts)))
  (define day (string->number (third parts)))
  (date->seconds (date 0 0 0 day month year 0 0 #f 0) #t))

(define-syntax-rule (define-dataset id path)
  (define id (load-stock-data path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indicator Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A helper predicate to check that a “date-close pair” is a pair where the
;; first element (a plot day) and the second element (the close value) are numbers.
(define (date-close-pair? p)
  (and (pair? p)
       (number? (first p))
       (number? (second p))))

(define/contract (date-close-pairs stock)
  (-> stock-data? (listof date-close-pair?))
  (map (λ (row)
         (list (seconds->plot-day (parse-date (first row))) 
               (list-ref row 4)))
       (stock-data-rows stock)))

(define/contract (compute-sma period pairs)
  (-> (and/c integer? (λ (n) (> n 0))) ; period must be a positive integer
      (listof date-close-pair?)
      (listof date-close-pair?))
  (define n (length pairs))
  (if (< n period)
      '()
      (for/list ([i (in-range 0 (- n period))])
        (let* ([window  (take (drop pairs i) period)]
               [average (/ (apply + (map second window)) period)]
               [date    (first (last window))])
          (list date average)))))

(define/contract (SMA period stock)
  (-> (and/c integer? (λ (n) (> n 0))) stock-data? (listof date-close-pair?))
  (compute-sma period (date-close-pairs stock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plotting System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (original-candle indicators
                                   #:label [label ""]
                                   #:volume [volume #t]
                                   #:colors [colors '("blue" "red" "green")])
  (-> list? 
      #:label string?
      #:volume boolean?
      #:colors (listof string?)
      list?)
  (for/list ([pts indicators]
             [color (in-list colors)])
    (lines pts #:color color #:label label)))

(define-for-syntax (split-keywords k-syn v-syn oc-set stx)
  (define k-list (syntax->list k-syn))
  (define v-list (syntax->list v-syn))
  (define oc-values (make-hash))
  (hash-set! oc-values '#:label (datum->syntax stx ""))
  (hash-set! oc-values '#:volume (datum->syntax stx #t))
  (hash-set! oc-values '#:colors (datum->syntax stx '("blue" "red" "green")))
  (define plot-kws '())
  (for ([k k-list] [v v-list])
    (if (member (syntax-e k) (map syntax-e (syntax->list oc-set)))
        (hash-set! oc-values (syntax-e k) v)
        (set! plot-kws (append plot-kws (list k v)))))
  (values (hash-ref oc-values '#:label)
          (hash-ref oc-values '#:volume)
          (hash-ref oc-values '#:colors)
          plot-kws))

(define-syntax (candle stx)
  (syntax-parse stx
    [(_ (~seq indicator:expr) ... (~seq k:keyword v:expr) ...)
     (define-values (oc-label oc-volume oc-colors plot-kws)
       (split-keywords #'(k ...) #'(v ...) #'(#:label #:volume #:colors) stx))
     #`(parameterize ([plot-new-window? #t]
                      [plot-x-ticks (date-ticks)]
                      [plot-x-tick-label-angle 30]
                      [plot-x-tick-label-anchor 'top-right])
         (plot (keyword-apply
                original-candle
                '(#:colors #:label #:volume)
                (list #,oc-colors #,oc-label #,oc-volume)
                (list (list #,@(syntax->list #'(indicator ...))))) ; Wrap indicators in a list
               #:x-label "Date"
               #,@plot-kws))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main DSL Usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (define-dataset AAPL "aapl.csv")
  (define-dataset MSFT "msft.csv")

  (candle
   (SMA 20 AAPL)
   (SMA 20 MSFT)
   #:label "Lines"
   #:title "AAPL vs MSFT 20-day SMA"
   #:colors '("red" "blue")
   #:y-label "Price (USD)"
   #:width 1200
   #:height 600))

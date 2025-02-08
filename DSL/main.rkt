#lang racket
(require plot            ; provides plot, lines, etc.
         racket/string   ; for string utilities
         "csv-reader.rkt")  ; our CSV reader module

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indicator Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Assume each data point is a list:
;;   (list timestamp open high low close volume adj-close)
;; where the close price is at index 4.

;; Extract closing prices from a stock-data structure.
(define (closing-prices stock)
  (map (lambda (row) (list-ref row 4))
       (stock-data-rows stock)))

;; Compute the Simple Moving Average (SMA) for a given period over a list of prices.
(define (compute-sma period prices)
  (define n (length prices))
  (if (< n period)
      '()  ; Not enough data to compute SMA
      (for/list ([i (in-range 0 (- n period))])
        (/ (apply + (take (drop prices i) period)) period))))

;; SMA indicator function.
;; Usage: (SMA 20 AAPL)
;; Returns a list of (x y) points for plotting.
(define (SMA period stock)
  (define prices (closing-prices stock))
  (define sma-values (compute-sma period prices))
  (define start-index (sub1 period)) ; The first computed SMA corresponds to index (period - 1)
  (for/list ([i (in-range (length sma-values))])
    (list (+ start-index i) (list-ref sma-values i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extendable Plotting System: candle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The `candle` function:
;;   - Accepts a list of indicator data (each a list of (x y) points).
;;   - Accepts optional keyword arguments:
;;         #:label  (for possible future legend support)
;;         #:volume (flag for volume plotting)
;;         #:colors (a list of colors to use for the indicator lines)
;;   - Returns a list of renderers (one for each indicator) that will be plotted.
(define (candle indicators 
                #:label [label ""] 
                #:volume [volume #t]
                #:colors [colors '("blue" "red" "green" "orange" "purple")])
  (define plots
    (for/list ([pts indicators] [color (in-list colors)])
      (lines pts #:color color)))
  (when volume
    (displayln "Volume plotting is not implemented yet."))
  plots)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main DSL Usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main  ; add this to enable GUI plotting

  (define AAPL (load-stock-data "aapl.csv"))
  
  ;; Optional: Display headers/data for debugging
  (displayln "\nCSV Headers:")
  (for-each displayln (stock-data-header AAPL))
  
  (displayln "\nFirst few data points:")
  (for-each displayln (take (stock-data-rows AAPL) 3))
  
  ;; Create the candle chart with indicators
  (define picture
    (candle (list (SMA 20 AAPL)
                  (SMA 50 AAPL))
            #:label "SMA 20 & 50"
            #:volume #f
            #:colors '("magenta" "cyan")))
  
  ;; Use plot/frame to open the plot in a new window
  (parameterize ([plot-new-window? #t])
    (plot
     picture
     #:title "AAPL SMA Chart"
     #:x-label "Days"
     #:y-label "Price")))

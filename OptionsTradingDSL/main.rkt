#lang racket

;; -----------------------------------------------------------------------------
;;  • Replaces compile‐time macros with plain Racket functions.
;;      – make-strategy/shortcut   → convenient one‐liner for canned spreads.
;;      – make-strategy            → fully‐specified legs using helpers (buy/sell).
;; -----------------------------------------------------------------------------

(require plot
         math/special-functions)
(require "private/functions.rkt")

;; -----------------------------------------------------------------------------
;; Public constructors
;; -----------------------------------------------------------------------------
(define (make-strategy/shortcut name
                                #:ticker ticker
                                #:ticker-price cp
                                #:quantity [qty 1]
                                #:volatility [vol 0.2]
                                #:risk-free-rate [rfr 0.05]
                                strategy-type . args)
  (assert symbol? ticker          'make-strategy "ticker must be a symbol")
  (assert positive-number? cp     'make-strategy "ticker-price must be > 0")
  (assert non‐neg-number? vol     'make-strategy "volatility must be ≥ 0")
  (assert non‐neg-number? rfr     'make-strategy "risk‐free rate must be ≥ 0")
  (assert positive-integer? qty 'make-strategy/shortcut "quantity must be a positive integer")

  (strategy name ticker cp #f vol rfr (expand-strategy-legs strategy-type args qty)))

(define (make-strategy name
                       #:ticker ticker
                       #:ticker-price cp
                       #:volatility [vol 0.2]
                       #:risk-free-rate [rfr 0.05]
                       . legs)

  (assert symbol? ticker          'make-strategy "ticker must be a symbol")
  (assert positive-number? cp     'make-strategy "ticker-price must be > 0")
  (assert non‐neg-number? vol     'make-strategy "volatility must be ≥ 0")
  (assert non‐neg-number? rfr     'make-strategy "risk‐free rate must be ≥ 0")

  (strategy name ticker cp #f vol rfr legs))



(define (graph-multiple-strategies-3d strategies
                                      #:min-price [min-price #f]
                                      #:max-price [max-price #f]
                                      #:min-days [min-days 1]
                                      #:max-days [max-days 60]
                                      #:price-step [price-step 5]
                                      #:day-step [day-step 2])
  (let-values ([(x-min x-max)
                (get-plot-bounds strategies min-price max-price)])
    (parameterize ([plot-new-window? #t])
      (plot3d
       (append
        (for/list ([strat-info strategies])
          (match-define (list strategy label color) strat-info)
          (surface3d
           (λ (x y)
             (total-strategy-value-at-time strategy x y))
           x-min x-max
           min-days max-days
           #:color color)))
       #:title "Option Strategy Value Over Time (3D)"
       #:x-label "Stock Price"
       #:y-label "Days Since Purchase"
       #:z-label "Profit/Loss"
       #:x-min x-min
       #:x-max x-max
       #:y-min min-days
       #:y-max max-days
       #:width 1400
       #:height 600))))

;; Main function
(define (graph-multiple-strategies-2d strategies
                                      #:min-price [min-price #f]
                                      #:max-price [max-price #f]
                                      #:days-since-purchase [days-since #f])
  (let-values ([(x-min x-max)
                (get-plot-bounds strategies min-price max-price)])
    (parameterize ([plot-new-window? #t])  
      (plot (append-map (lambda (strat-info)
                          (match-define
                            (list strategy label color) strat-info)
                          (make-single-strategy-plot
                           strategy label color x-min
                           x-max #:days-since-purchase days-since))
                        strategies)
            #:title "Option Strategy Comparison"
            #:x-label "Stock Price"
            #:y-label "Profit/Loss"
            #:x-min x-min
            #:x-max x-max
            #:width 1400
            #:height 600
            #:legend-anchor 'outside-right))))

(define (graph-decision strategy-triplets
                        #:3d [use-3d? #f]
                        #:min-price [min-price #f]
                        #:max-price [max-price #f]
                        #:min-days [min-days 1]
                        #:max-days [max-days #f]
                        #:price-step [price-step 5]
                        #:day-step [day-step 2]
                        #:days-since-purchase [days-since #f])
  (define (get-expiration-max strat)
    (apply max (map option-leg-expiration (filter option-leg?
                                                  (strategy-legs strat)))))

  (define computed-max-days
    (or max-days
        (apply max
               (map (lambda (triplet)
                      (define strat (first triplet))
                      (get-expiration-max strat))
                    strategy-triplets))))

  (if use-3d?
      (graph-multiple-strategies-3d
       strategy-triplets
       #:min-price min-price
       #:max-price max-price
       #:min-days min-days
       #:max-days computed-max-days
       #:price-step price-step
       #:day-step day-step)
      (graph-multiple-strategies-2d
       strategy-triplets
       #:min-price min-price
       #:max-price max-price
       #:days-since-purchase days-since)))




(provide
  make-strategy/shortcut
  make-strategy
  graph-multiple-strategies-3d
  graph-multiple-strategies-2d
  graph-decision)




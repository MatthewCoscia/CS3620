#lang racket

;; -----------------------------------------------------------------------------
;;  • Replaces compile‐time macros with plain Racket functions.
;;      – make-strategy/shortcut   → convenient one‐liner for canned spreads.
;;      – make-strategy            → fully‐specified legs using helpers (buy/sell).
;; -----------------------------------------------------------------------------

(require "private/functions.rkt")
;; Use dynamic-require for lazy loading
(define gui-module '("private/gui.rkt"))

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

;; Define function that gets the plot utility dynamically
(define (get-render-3d-function)
  (dynamic-require (car gui-module) 'render-multiple-strategies-3d))

(define (get-render-2d-function)
  (dynamic-require (car gui-module) 'render-multiple-strategies-2d))

;; Define public interfaces with lazy loading
(define (graph-multiple-strategies-3d strategies
                                      #:min-price [min-price #f]
                                      #:max-price [max-price #f]
                                      #:min-days [min-days 1]
                                      #:max-days [max-days 60]
                                      #:price-step [price-step 5]
                                      #:day-step [day-step 2])
  (define render-3d (get-render-3d-function))
  (render-3d strategies min-price max-price 
             min-days max-days price-step day-step))

(define (graph-multiple-strategies-2d strategies
                                      #:min-price [min-price #f]
                                      #:max-price [max-price #f]
                                      #:days-since-purchase [days-since #f])
  (define render-2d (get-render-2d-function))
  (render-2d strategies min-price max-price days-since))

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


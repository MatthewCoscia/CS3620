#lang racket
;;Grammar:

;; STRATEGY
(define-option-strategy strategy-name
  #:ticker 'SYMBOL
  #:current-price PRICE
  #:safe-mode BOOLEAN
  [#:volatility VOLATILITY] ; default 0.3
  [#:risk-free-rate RFR] ; default 0.05
  TRADE ...)


; TRADE
(ACTION QTY TYPE #:strike STRIKE #:expiration EXPIRATION [#:premium PREMIUM])


(graph-multiple-strategies
 (list STRATEGY ...)
 [#:min-price MIN]
 [#:max-price MAX])
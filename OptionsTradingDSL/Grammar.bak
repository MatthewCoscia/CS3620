#lang racket
;;Grammar:

;; STRATEGY
(define-option-strategy strategy-name
  #:ticker 'SYMBOL
  #:ticker-price PRICE
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



(define-option-strategy safe-strat
  #:ticker 'GOOG
  #:ticker-price 145.75
  #:safe-mode #t
  (sell 1 call #:strike 150 #:expiration 30)
  (buy  1 call #:strike 150 #:expiration 30) 
  (sell 1 put  #:strike 140 #:expiration 30)
  (buy  1 put  #:strike 140 #:expiration 30))
;;  ---->
#|
(define safe-strat
  (hash
   'ticker-value
   ''GOOG
   'ticker-price
   145.75
   'safe-mode
   #t
   'volatility
   0.2
   'risk-free-rate
   0.05
   'legs (list (list 'sell 1 'call 150 30 #f)
            (list 'buy 1 'call 150 30 #f)
            (list 'sell 1 'put 140 30 #f)
            (list 'buy 1 'put 140 30 #f))))
|#

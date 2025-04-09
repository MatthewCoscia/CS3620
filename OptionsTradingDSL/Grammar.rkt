#lang racket
;;Grammar:

;; Example Expansion
#|
(define-option-strategy call-alone
  #:ticker 'AAPL
  #:ticker-price 150
  (buy 1 call #:strike 145 #:expiration 10 #:premium 1))
(define call-alone
  (strategy
   'call-alone                ; name
   'AAPL                      ; ticker symbol
   150                        ; current ticker price
   #f                         ; safe-mode (unused in longform)
   0.2                        ; default volatility
   0.05                       ; default risk-free rate
   (list
    (option-leg 'buy 1 'call 145 10 1)))) ; ← expanded leg
|#

; Example Structs:
#|
(struct strategy (name ticker ticker-price safe-mode volatility
                      risk-free-rate legs)
  #:transparent)

Expanded:

(strategy
 'bull-call-spread           ; name
 'AAPL                       ; ticker
 150                         ; ticker-price
 #f                          ; safe-mode
 0.3                         ; volatility
 0.02                        ; risk-free-rate
 (list (option-leg 'buy 1 'call 140 30 #f)))  ; legs

(struct option-leg (action qty type strike expiration premium)
  #:transparent)

Expanded:

(option-leg 'buy 1 'call 140 30 #f)

|#

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

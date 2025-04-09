#lang racket

;; Purpose of DSL

;; Options Trading DSL
;;   - Define easily
;;   - Visualize
;;   - Keep yourself out of debt
;;       - error checking
;;       - safe mode















;; Demo

;;   - 2d view
;;   - 3d view












;; Example inupt

;; Two Ways to define the same thing:
(define-option-strategy call-debit-spread
  #:ticker 'TSLA
  #:ticker-price 250.50
  #:safe-mode #t
  (buy 10 call #:strike 250 #:expiration 3) 
  (sell 10 call  #:strike 255 #:expiration 3))


(define-option-strategy call-debit-spread-shortened
  #:ticker 'TSLA
  #:ticker-price 250.50
  #:safe-mode #t
  #:quantity 10
  (call-debit-spread 250 255 3))


(define-option-strategy collar-spread
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  ((buy 100 shares)
   buy 1  put  #:strike 140  #:expiration 4)
  (sell 1 call #:strike 150 #:expiration 4)

(define-option-strategy collar-shortened
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  (collar 140 150 7))


#|
Supported Strategies
--------------------

Strategy                | Required Arguments                                         | Legs Generated
------------------------|------------------------------------------------------------|-----------------------------------------------------------
call-debit-spread       | (buy-strike sell-strike expiration)                        | Buy call @ strike1, Sell call @ strike2
put-debit-spread        | (buy-strike sell-strike expiration)                        | Buy put @ strike1, Sell put @ strike2
butterfly-spread        | (low-strike mid-strike high-strike expiration)             | Buy call @ k1, Sell 2 calls @ k2, Buy call @ k3
call-credit-spread      | (sell-strike buy-strike expiration)                        | Sell call @ strike1, Buy call @ strike2
put-credit-spread       | (sell-strike1 buy-strike expiration)                       | Sell put @ strike1, Buy put @ strike2
iron-condor             | (strike1-g1 strike2-g1 strike3-g2 strike4-g2 expiration)   | Buy put @ k1, Sell put @ k2, Sell call @ k3, Buy call @ k4
iron-butterfly          | (low-strike mid-strike high-strike expiration)             | Buy put @ k1, Sell put @ k2, Sell call @ k2, Buy call @ k3
long-straddle           | (strike expiration)                                        | Buy call + put @ same strike
long-strangle           | (put-strike call-strike expiration)                        | Buy put @ put-strike, Buy call @ call-strike
covered-call            | (strike expiration)                                        | Buy 100 shares, Sell 1 call
collar                  | (long-put-strike short-call-strike expiration)             | Buy 100 shares, Buy 1 put, Sell 1 call
diagonal-call-spread    | (near-strike near-expiration far-strike far-expiration)    | Sell near call, Buy far call

..... (Eventual goal is allow users to define their own)
|#

  



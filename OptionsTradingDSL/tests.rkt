#lang racket
(require (for-syntax racket/base
                     racket/match 
                     racket/syntax
                     syntax/parse)
         racket/date
         plot
         rackunit
         rackunit/text-ui
         math/special-functions)

(require "updated_syntax.rkt")

;; Example that would error
(define-option-strategy risky-strat
  #:ticker 'TSLA
  #:ticker-price 250.50
  (buy 1 call #:strike 300 #:expiration 30)  ;; Naked call
  (sell 1 call  #:strike 200 #:expiration 30)) ;; Naked put

;; Valid covered strategy
(define-option-strategy safe-strat
  #:ticker 'GOOG
  #:ticker-price 145.75
  (sell 1 call #:strike 150 #:expiration 30)
  (buy  1 call #:strike 150 #:expiration 30)  ;; Covers call
  (sell 1 put  #:strike 140 #:expiration 30)
  (buy  1 put  #:strike 140 #:expiration 30)) ;; Covers put

(define-option-strategy bullish-strat
  #:ticker 'AAPL
  #:ticker-price 145.75
  (buy 1 call #:strike 140 #:expiration 30)  ;; Lower strike, long call
  (sell 1 call #:strike 150 #:expiration 30)) ;; Higher strike, short call




(define-option-strategy high-vol-strat
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:volatility 0.3  ;; 30% volatility
  #:risk-free-rate 0.02  ;; 2% risk-free rate
  (buy 1 call #:strike 140 #:expiration 30)
  (sell 1 call #:strike 150 #:expiration 30))

(define-option-strategy high-vol-strat-prem
  #:ticker 'AAPL
  #:ticker-price 280.75
  #:volatility 0.3  ;; 30% volatility
  #:risk-free-rate 0.02  ;; 2% risk-free rate
  (buy 1 call #:strike 240 #:expiration 30 #:premium 7.50)
  (sell 1 call #:strike 320 #:expiration 30 #:premium 5.00))



(define (≈ a b tol) (< (abs (- a b)) tol))

(define tol 0.001)

(define-test-suite premium-tests
  (check-true (≈ (calculate-premium 140 145.75 0.0822 0.02 0.3 'call)
                 8.4563 tol))
  (check-true (≈ (calculate-premium 150 145.75 0.0822 0.02 0.3 'call)
                 3.3159 tol))
  (check-true (≈ (calculate-premium 140 145.75 0.0822 0.02 0.3 'put)
                 2.4763 tol))
  (check-true (≈ (calculate-premium 150 145.75 0.0822 0.02 0.3 'put)
                 7.3195 tol))
  (check-true (≈ (calculate-premium 140 145.75 1 0.05 0.2 'call)
                 18.5054 tol))
  (check-true (≈ (calculate-premium 150 145.75 1 0.05 0.2 'put)
                 10.0195 tol)))

(define-test-suite option-value-tests
  ;; All these are evaluated at expiration (days-since = total)
  (check-= (option-value-at-time 110 100 'buy 'call 1 5 0 0 365 365 100) 500 0.001)
  (check-= (option-value-at-time 110 100 'sell 'call 1 5 0 0 365 365 100) -500 0.001)
  (check-= (option-value-at-time 90 100 'buy 'put 1 3 0 0 365 365 100) 700 0.001)
  (check-= (option-value-at-time 90 100 'sell 'put 1 3 0 0 365 365 100) -700 0.001)
  (check-= (option-value-at-time 90 100 'buy 'call 1 5 0 0 365 365 100) -500 0.001)
  (check-= (option-value-at-time 100 100 'buy 'call 1 2 0 0 365 365 100) -200 0.001)
  (check-= (option-value-at-time 100 100 'sell 'call 1 2 0 0 365 365 100) 200 0.001)
  (check-= (option-value-at-time 110 100 'buy 'call 2 5 0 0 365 365 100) 1000 0.001)
  (check-= (option-value-at-time 105 100 'buy 'call 1 #f 0.05 0.3 0 365 100)
         327.38 0.5))

(define-test-suite total-strategy-value-tests

  ;; Test long call: In-the-money
  (check-= 
   (total-strategy-value-at-time
    (strategy 'test 'AAPL 100 #f 0.3 0.05
              (list (option-leg 'buy 1 'call 90 30 #f)))
    105 ;; current stock price
    0)  ;; 0 days passed → full expiration
   (option-value-at-time 105 90 'buy 'call 1 #f 0.05 0.3 0 30 100)
   0.01)

  ;; Test long put: In-the-money
  (check-=
   (total-strategy-value-at-time
    (strategy 'test2 'AAPL 100 #f 0.3 0.05
              (list (option-leg 'buy 1 'put 110 30 #f)))
    100 0)
   (option-value-at-time 100 110 'buy 'put 1 #f 0.05 0.3 0 30 100))

  ;; Test covered call (long shares + short call)
  (check-=
   (total-strategy-value-at-time
    (strategy 'covered 'AAPL 100 #f 0.2 0.05
              (list
               (shares-leg 'buy 100)
               (option-leg 'sell 1 'call 110 10 #f)))
    115
    0)
   (+ (share-payoff 115 'buy 100 100)
      (option-value-at-time 115 110 'sell 'call 1 #f 0.05 0.2 0 10 100)))

  ;; Test short put (out-of-the-money)
  (check-=
   (total-strategy-value-at-time
    (strategy 'short-put 'AAPL 100 #f 0.25 0.05
              (list (option-leg 'sell 1 'put 90 30 #f)))
    100
    0)
   (option-value-at-time 100 90 'sell 'put 1 #f 0.05 0.25 0 30 100))

  ;; Test far-from-expiration value
  (check-=
   (total-strategy-value-at-time
    (strategy 'long-call-time 'AAPL 100 #f 0.4 0.05
              (list (option-leg 'buy 1 'call 120 365 #f)))
    105
    0) ;; day 0
   (option-value-at-time 105 120 'buy 'call 1 #f 0.05 0.4 0 365 100))
)



;; Run all tests with verbose output
(displayln "Running Premium Tests:")
(run-tests premium-tests 'verbose)
(newline)
(displayln "Running Option Value Tests:")
(run-tests option-value-tests 'verbose)
(newline)
(displayln "Running Total Option Value Tests:")
(run-tests total-strategy-value-tests 'verbose)
(newline)
#lang racket
(require rackunit)

(require "private/functions.rkt")
(require "main.rkt")



;; -----------------------------------------------------------------------------
;; Example usage
;; -----------------------------------------------------------------------------
(define bullish-strat-shortened
  (make-strategy/shortcut 'bullish-strat-shortened
                          #:ticker 'AAPL
                          #:ticker-price 145.75
                          #:quantity 2
                          `call-debit-spread 140 150 7))

(define collar-shortened
  (make-strategy/shortcut 'collar-shortened
                          #:ticker 'AAPL
                          #:ticker-price 145.75
                          `collar 140 150 7))

(define decaying-call-spread
  (make-strategy 'decaying-call-spread
                 #:ticker 'AAPL
                 #:ticker-price 150
                 #:volatility 0.3
                 #:risk-free-rate 0.02
                 (buy 1 call #:strike 145 #:expiration 1000)
                 (sell 1 call #:strike 155 #:expiration 1000)))


(define covered-call-test
  (make-strategy 'covered-call-test
                 #:ticker 'AAPL
                 #:ticker-price 150
                 (buy 100 shares)
                 (sell 1 call #:strike 160 #:expiration 30)))

(define protective-put-test
  (make-strategy 'protective-put-test
                 #:ticker 'AAPL
                 #:ticker-price 150
                 (buy 100 shares)
                 (buy 1 put #:strike 140 #:expiration 30)))

(define synthetic-short-put
  (make-strategy 'synthetic-short-put
                 #:ticker 'AAPL
                 #:ticker-price 150
                 (sell 100 shares)
                 (buy 1 call #:strike 150 #:expiration 30)))



#;
(define bad-call-debit
  (make-strategy/shortcut 'bad-call-debit
                          #:ticker 'AAPL
                          #:ticker-price 150
                          #:quantity 1
                          'call-debit-spread 160 150 30))

;; helper that produces a test‐case in one line
(define (expect-fail desc thunk)
  (test-case desc
    (check-exn exn:fail? thunk)))

(define-syntax-rule (check-≈ expr expected tol)
  (check-= expr expected tol))

(define validation-tests
  (test-suite
   "strategy runtime validation"

   ;; 0. positive build
   (test-case "valid strategy builds"
     (check-true
      (strategy? (make-strategy/shortcut 'ok
                                         #:ticker 'AAPL
                                         #:ticker-price 150
                                         'call-debit-spread 140 150 30))))

   ;; 1. header checks
   (expect-fail "ticker must be symbol"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker "AAPL"
                                   #:ticker-price 150
                                   'call-debit-spread 140 150 30)))

   (expect-fail "ticker-price > 0"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price -1
                                   'call-debit-spread 140 150 30)))

   (expect-fail "quantity > 0"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   #:quantity 0
                                   'call-debit-spread 140 150 30)))

   ;; 2. shortcut ordering / distinctness
   (expect-fail "call‐debit: buy < sell"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'call-debit-spread 160 150 30)))

   (expect-fail "put‐debit: buy > sell"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'put-debit-spread 140 150 30)))

   (expect-fail "call‐credit: sell ≤ buy"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'call-credit-spread 140 150 30)))

   (expect-fail "put‐credit: sell ≥ buy"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'put-credit-spread 150 140 30)))

   (expect-fail "butterfly: duplicate strikes"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'butterfly-spread 150 150 160 30)))

   (expect-fail "butterfly: out‐of‐order strikes"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'butterfly-spread 160 150 140 30)))

   (expect-fail "iron‐condor: strikes must ascend"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'iron-condor 130 120 140 150 30)))

   (expect-fail "long‐strangle: put < call"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'long-strangle 160 140 30)))

   ;; 3. leg constructor validation
   (expect-fail "negative quantity in buy"
     (λ () (make-strategy 'bad-leg
                          #:ticker 'AAPL
                          #:ticker-price 150
                          (buy -1 call #:strike 100 #:expiration 30))))

   (expect-fail "non‐numeric strike"
     (λ () (make-strategy 'bad-leg
                          #:ticker 'AAPL
                          #:ticker-price 150
                          (buy 1 call #:strike 'foo #:expiration 30))))
))


(define (≈ a b tol) (< (abs (- a b)) tol))
(define tol 0.001)

;; ------------------------------------------------------------
;; Premium tests
;; ------------------------------------------------------------
(define premium-tests
  (test-suite
   "Black–Scholes premium values (tolerance 1e‐3)"
   (check-true (≈ (calculate-premium 140 145.75 0.0822 0.02 0.3 'call) 8.4563 tol))
   (check-true (≈ (calculate-premium 150 145.75 0.0822 0.02 0.3 'call) 3.3159 tol))
   (check-true (≈ (calculate-premium 140 145.75 0.0822 0.02 0.3 'put)  2.4763 tol))
   (check-true (≈ (calculate-premium 150 145.75 0.0822 0.02 0.3 'put)  7.3195 tol))
   (check-true (≈ (calculate-premium 140 145.75 1       0.05 0.2 'call) 18.5054 tol))
   (check-true (≈ (calculate-premium 150 145.75 1       0.05 0.2 'put)  10.0195 tol))))

;; ------------------------------------------------------------
;; Option‐value tests
;; ------------------------------------------------------------
(define option-value-tests
  (test-suite
   "Black–Scholes value vs. payoff at expiry"
   (check-= (option-value-at-time 110 100 'buy  'call 1 5 0 0 365 365 100)  500 0.001)
   (check-= (option-value-at-time 110 100 'sell 'call 1 5 0 0 365 365 100) -500 0.001)
   (check-= (option-value-at-time  90 100 'buy  'put  1 3 0 0 365 365 100)  700 0.001)
   (check-= (option-value-at-time  90 100 'sell 'put  1 3 0 0 365 365 100) -700 0.001)
   (check-= (option-value-at-time  90 100 'buy  'call 1 5 0 0 365 365 100) -500 0.001)
   (check-= (option-value-at-time 100 100 'buy  'call 1 2 0 0 365 365 100) -200 0.001)
   (check-= (option-value-at-time 100 100 'sell 'call 1 2 0 0 365 365 100)  200 0.001)
   (check-= (option-value-at-time 110 100 'buy  'call 2 5 0 0 365 365 100) 1000 0.001)
   ;; time value example
   (check-= (option-value-at-time 105 100 'buy 'call 1 #f 0.05 0.3 0 365 100)
            327.38 0.5)))

;; ------------------------------------------------------------
;; Total‐strategy tests
;; ------------------------------------------------------------
(define total-strategy-value-tests
  (test-suite
   "total-strategy-value-at-time consistency"
   (check-≈
    (total-strategy-value-at-time
     (strategy 'test 'AAPL 100 #f 0.3 0.05
               (list (option-leg 'buy 1 'call 90 30 #f)))
     105 0)
    (option-value-at-time 105 90 'buy 'call 1 #f 0.05 0.3 0 30 100)
    0.01)

   (check-≈
    (total-strategy-value-at-time
     (strategy 'test2 'AAPL 100 #f 0.3 0.05
               (list (option-leg 'buy 1 'put 110 30 #f)))
     100 0)
    (option-value-at-time 100 110 'buy 'put 1 #f 0.05 0.3 0 30 100)
    0.01)

   (check-≈
    (total-strategy-value-at-time
     (strategy 'covered 'AAPL 100 #f 0.2 0.05
               (list (shares-leg 'buy 100)
                     (option-leg 'sell 1 'call 110 10 #f)))
     115 0)
    (+ (share-payoff 115 'buy 100 100)
       (option-value-at-time 115 110 'sell 'call 1 #f 0.05 0.2 0 10 100))
    0.01)

   (check-≈
    (total-strategy-value-at-time
     (strategy 'short-put 'AAPL 100 #f 0.25 0.05
               (list (option-leg 'sell 1 'put 90 30 #f)))
     100 0)
    (option-value-at-time 100 90 'sell 'put 1 #f 0.05 0.25 0 30 100)
    0.01)

   (check-≈
    (total-strategy-value-at-time
     (strategy 'long-call-time 'AAPL 100 #f 0.4 0.05
               (list (option-leg 'buy 1 'call 120 365 #f)))
     105 0)
    (option-value-at-time 105 120 'buy 'call 1 #f 0.05 0.4 0 365 100)
    0.01)))

(define all-tests
  (test-suite
   "All option‐strategy tests"
   validation-tests
   premium-tests
   option-value-tests
   total-strategy-value-tests))





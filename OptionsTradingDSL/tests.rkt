#lang racket
;; Example that would error
(define-option-strategy risky-strat
  #:ticker 'TSLA
  #:ticker-price 250.50
  #:safe-mode #t
  (buy 1 call #:strike 300 #:expiration 30)  ;; Naked call
  (sell 1 call  #:strike 200 #:expiration 30)) ;; Naked put

;; Valid covered strategy
(define-option-strategy safe-strat
  #:ticker 'GOOG
  #:ticker-price 145.75
  #:safe-mode #t
  (sell 1 call #:strike 150 #:expiration 30)
  (buy  1 call #:strike 150 #:expiration 30)  ;; Covers call
  (sell 1 put  #:strike 140 #:expiration 30)
  (buy  1 put  #:strike 140 #:expiration 30)) ;; Covers put

(define-option-strategy bullish-strat
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  (buy 1 call #:strike 140 #:expiration 30)  ;; Lower strike, long call
  (sell 1 call #:strike 150 #:expiration 30)) ;; Higher strike, short call




(define-option-strategy high-vol-strat
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  #:volatility 0.3  ;; 30% volatility
  #:risk-free-rate 0.02  ;; 2% risk-free rate
  (buy 1 call #:strike 140 #:expiration 30)
  (sell 1 call #:strike 150 #:expiration 30))

(define-option-strategy high-vol-strat-prem
  #:ticker 'AAPL
  #:ticker-price 280.75
  #:safe-mode #t
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

(define-test-suite payoff-tests
  (check-= (option-payoff 110 100 'buy 'call 1 5 0 0 365 100) 5 0.001)
  (check-= (option-payoff 110 100 'sell 'call 1 5 0 0 365 100) -5 0.001)
  (check-= (option-payoff 90 100 'buy 'put 1 3 0 0 365 100) 7 0.001)
  (check-= (option-payoff 90 100 'sell 'put 1 3 0 0 365 100) -7 0.001)
  (check-= (option-payoff 90 100 'buy 'call 1 5 0 0 365 100) -5 0.001)
  (check-= (option-payoff 100 100 'buy 'call 1 2 0 0 365 100) -2 0.001)
  (check-= (option-payoff 100 100 'sell 'call 1 2 0 0 365 100) 2 0.001)
  (check-= (option-payoff 110 100 'buy 'call 2 5 0 0 365 100) 10 0.001)
  (check-= (option-payoff 105 100 'buy 'call 1 #f 0.05 0 365 100) 0.123 0.01))


;; Example Fails

(define (fails-to-compile? expr)
  (with-handlers ([exn:fail? (λ (_) #t)]) ;; If an exception occurs, return #t
    (eval expr)  ;; Try to evaluate the expression
    #f))         ;; If it compiles, return #f (which is a failure in our test)

(define-test-suite safe-mode-failure-tests

  ;; Cannot sell more contracts than total purchased (no over-leveraging)
  (check-true (fails-to-compile?
               '(define-option-strategy over-leveraged
                  #:ticker 'TSLA
                  #:ticker-price 700
                  #:safe-mode #t
                  (buy 2 call #:strike 750 #:expiration 30)
                  (sell 5 call #:strike 750 #:expiration 30)))
              ;; Selling more than buying
              "Safe mode: Should fail because more options
\are sold than bought.")

  ;; Strike price must be within 30% of current price
  (check-true (fails-to-compile?
               '(define-option-strategy too-far-otm
                  #:ticker 'NFLX
                  #:ticker-price 500
                  #:safe-mode #t
                  (buy 1 call #:strike 700 #:expiration 60)))
              ;; 40% away from current price
              "Safe mode: Should fail because strike price is too high.")

  (check-true (fails-to-compile?
               '(define-option-strategy too-far-it
                  #:ticker 'NFLX
                  #:ticker-price 500
                  #:safe-mode #t
                  (buy 1 put #:strike 250 #:expiration 60)))
              ;; Strike too far from current price
              "Safe mode: Should fail because strike price is too low.")

  ;; Naked short calls or puts are not allowed
  (check-true (fails-to-compile?
               '(define-option-strategy naked-call
                  #:ticker 'AMZN
                  #:ticker-price 3000
                  #:safe-mode #t
                  (sell 1 call #:strike 3100 #:expiration 30)))
              ;; No corresponding "buy"
              "Safe mode: Should fail because selling a naked call.")

  (check-true (fails-to-compile?
               '(define-option-strategy naked-put
                  #:ticker 'GOOG
                  #:ticker-price 2800
                  #:safe-mode #t
                  (sell 1 put #:strike 2700 #:expiration 30)))
              ;; No corresponding "buy"
              "Safe mode: Should fail because selling a naked put.")
  )

;; Run all tests with verbose output
(displayln "Running Premium Tests:")
(run-tests premium-tests 'verbose)
(newline)
(displayln "Running Payoff Tests:")
(run-tests payoff-tests 'verbose)
(newline)
(displayln "Compilation Error Tests:")
(run-tests safe-mode-failure-tests 'verbose)
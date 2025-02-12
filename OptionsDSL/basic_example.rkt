#lang racket

(require plot)
(require (for-syntax syntax/parse))

;; Define helpers to create call and put option payoff functions
(define (graph-strategy strike premium type)
  (lambda (x)
    (cond
      [(eq? type 'call) (- (max 0 (- x strike)) premium)]  ; for calls
      [(eq? type 'put)  (- (max 0 (- strike x)) premium)])))  ; for puts

;; Helper function to infer option type from name
(define-for-syntax (infer-type name-stx)
  (let* ([name-str (symbol->string (syntax->datum name-stx))]
         [first-three (substring name-str 0 (min 3 (string-length name-str)))])
    (if (string-ci=? first-three "cal")
        #'call  ;; Return as a syntax object, not a raw symbol
        (if (string-ci=? first-three "put")
            #'put
            (raise-syntax-error 'define-option "Option name does not start with 'call' or 'put'" name-stx)))))

(define-syntax (define-option stx)
  (syntax-parse stx
    [(_ name:id #:strike strike 
                #:current-price price
                #:expiration expiration)
     (let ([type (infer-type #'name)]) ;; Call the helper function at syntax-time
       #`(define name
           (let ([strategy (graph-strategy strike 
                                           (- price strike)  ;; initial premium calculation
                                           '#,type)])  ;; Ensure `type` is quoted correctly
             (lambda (x) (strategy x)))))]))

;; Define syntax for creating a strategy with an arbitrary number of options
(define-syntax-rule (define-option-strategy name option ...)
  (define name (lambda () (graph-strategy-multi (list (option) ...)))))

;; Function to graph multiple options together
(define (graph-strategy-multi options)
  (define (payoff x)
    (apply + (map (lambda (opt) (opt x)) options)))
  (plot (function payoff 60 250) #:title "Option Strategy Payoff"))

;; Example Usage:
(define-option call-1 #:strike 100 #:current-price 101 #:expiration 10)
(define-option put-1  #:strike 100 #:current-price 99  #:expiration 10)
(define-option-strategy my-strategy call-1 put-1)

(my-strategy)  ;; This should now call the strategy and plot the graph

#|
Option Pricing
Black-Scholes Model
Binomial Options Pricing Model:
Monte Carlo Simulation:
|#
#lang racket
(require (for-syntax syntax/parse)
         racket/date)

(begin-for-syntax
  (define-syntax-class action
    (pattern (~or buy sell)))
  
  (define-syntax-class option-type
    (pattern (~or call put)))

  (define-syntax-class positive-whole-qty
    #:description "positive whole number"
    (pattern q:expr
             #:fail-when (let ([v (syntax-e #'q)])
                           (not (and (exact-positive-integer? v)
                                     (<= v 3650))))
             "must be positive integer ≤ 3650")))




(define-syntax (define-option-strategy stx)
  (syntax-parse stx
    [(_ strategy-name:id 
        #:ticker ticker:expr
        #:current-price cp:expr
        (action:action qty:positive-whole-qty 
                       type:option-type 
                       #:strike s:expr
                       #:expiration exp:number) ...)
     
     #:with (action-sym ...) (map (λ (a) (datum->syntax #f (syntax->datum a))) 
                                  (syntax->list #'(action ...)))
     #:with (type-sym ...) (map (λ (t) (datum->syntax #f (syntax->datum t))) 
                                (syntax->list #'(type ...)))
     #:with (exp-spec ...) (syntax->list #'(exp ...))
     #'(define strategy-name
         (hash 'ticker ticker
               'current-price cp
               'legs (list (list 'action-sym qty 'type-sym s 'expiration exp) ...)))]))


;; Example usage
(define-option-strategy AAPL-strat
  #:ticker 'AAPL
  #:current-price 182.52
  (buy 1 call #:strike 200 #:expiration 20)
  (sell 1 put #:strike 200 #:expiration 20 ))

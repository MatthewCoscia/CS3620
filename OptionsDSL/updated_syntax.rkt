#lang racket
(require (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class action
    (pattern (~or buy sell)))
  
  (define-syntax-class option-type
    (pattern (~or call put)))

  (define-syntax-class positive-whole-qty
    #:description "positive whole quantity"
    (pattern q:expr
             #:fail-when (not (and
                                   (integer? (syntax-e #'q))
                                   (number? (syntax-e #'q)) 
                                   (positive? (syntax-e #'q))))
             "quantity must be a positive whole number literal"))

  (define-syntax-class positive-strike
    #:description "positive strike"
    (pattern q:expr
             #:fail-when (not (and
                                   (number? (syntax-e #'q)) 
                                   (positive? (syntax-e #'q))))
             "strike must be a positive number literal")))

(define-syntax (define-option-strategy stx)
  (syntax-parse stx
    [(_ strategy-name:id 
        #:ticker ticker:id
        #:ticker-price current-price:expr
        (action:action qty:positive-whole-qty
                       type:option-type
                       #:strike strike:positive-strike) ...)
     
     #:with ticker-symbol (datum->syntax #'ticker (symbol->string (syntax->datum #'ticker)))
     #:with (action-sym ...) (map (λ (a) (datum->syntax #f (syntax->datum a))) 
                                  (syntax->list #'(action ...)))
     #:with (type-sym ...) (map (λ (t) (datum->syntax #f (syntax->datum t))) 
                                (syntax->list #'(type ...)))
     #'(define strategy-name
         (hash 'ticker ticker-symbol
               'current-price current-price
               'legs (list (list 'action-sym qty 'type-sym strike) ...)))]))


(define-option-strategy my-strat
  #:ticker AAPL
  #:ticker-price 100
  (buy 1 call #:strike 100)
  (sell 1 call #:strike 105)
  (buy 1 put #:strike 95))
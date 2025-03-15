

#lang racket
(require (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class action
    (pattern (~or buy sell)))
  
  (define-syntax-class option-type
    (pattern (~or call put)))

  ; Move positive-qty inside begin-for-syntax
  (define-syntax-class positive-qty
    #:description "positive quantity"
    (pattern q:expr
             #:fail-when (not (and
                                   (integer? (syntax-e #'q))
                                   (number? (syntax-e #'q)) 
                                   (positive? (syntax-e #'q))))
             "quantity must be a positive whole number literal")))

(define-syntax (define-option-strategy stx)
  (syntax-parse stx
    [(_ strategy-name:id 
        (action:action qty:positive-qty type:option-type #:strike strike:expr) ...)
     
     #:with (action-sym ...) (map (λ (a) (datum->syntax #f (syntax->datum a))) 
                                  (syntax->list #'(action ...)))
     #:with (type-sym ...) (map (λ (t) (datum->syntax #f (syntax->datum t))) 
                                (syntax->list #'(type ...)))
     #'(define strategy-name
         (list (list 'action-sym qty 'type-sym strike) ...))]))


(define-option-strategy my-strat
  (buy 1 call #:strike 100)
  (sell 1 call #:strike 105)
  (buy 1 put #:strike 95))
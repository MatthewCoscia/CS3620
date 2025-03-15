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
                           (not (exact-positive-integer? v)))
             "must be positive integer"))
  
  (define-syntax-class ticker-symbol
    #:description "ticker symbol"
    (pattern t:expr
             #:when (let ([v (syntax->datum #'t)])
                      (and (pair? v) 
                           (eq? (car v) 'quote)
                           (symbol? (cadr v))))
             #:with ticker-value (let ([v (syntax->datum #'t)])
                                   (cadr v))))
  
  (define-syntax-class positive-price
    #:description "positive price"
    (pattern p:expr
             #:fail-when (let ([v (syntax-e #'p)])
                           (and (number? v)
                                (not (positive? v))))
             "price must be positive"))
             
  (define-syntax-class safe-mode
    #:description "safety mode setting"
    (pattern (~or #t #f))))

(define-syntax (define-option-strategy stx)
  (syntax-parse stx
    [(_ strategy-name:id 
        #:ticker ticker:ticker-symbol
        #:current-price cp:positive-price
        #:safe-mode safe:safe-mode
        (action:action qty:positive-whole-qty 
                       type:option-type 
                       #:strike s:expr 
                       #:expiration exp:number) ...)
     
     #:with (action-sym ...) (map (λ (a) (datum->syntax #f (syntax->datum a))) 
                                  (syntax->list #'(action ...)))
     #:with (type-sym ...) (map (λ (t) (datum->syntax #f (syntax->datum t))) 
                                (syntax->list #'(type ...)))
     
     ;; Combined check for both naked calls and puts
     #:fail-when (and (equal? (syntax-e #'safe) #t)
                      (or
                       (for/or ([a (syntax->list #'(action ...))]
                                [t (syntax->list #'(type ...))])
                         (and (eq? (syntax-e a) 'sell)
                              (eq? (syntax-e t) 'put)
                              (not (for/or ([a2 (syntax->list #'(action ...))]
                                            [t2 (syntax->list #'(type ...))]
                                            [q2 (syntax->list #'(qty ...))])
                                     (and (eq? (syntax-e a2) 'buy)
                                          (eq? (syntax-e t2) 'put)
                                          (>= (syntax-e q2) 1))))))

                       (for/or ([a (syntax->list #'(action ...))]
                                [t (syntax->list #'(type ...))])
                         (and (eq? (syntax-e a) 'sell)
                              (eq? (syntax-e t) 'call)
                              (not (for/or ([a2 (syntax->list #'(action ...))]
                                            [t2 (syntax->list #'(type ...))]
                                            [q2 (syntax->list #'(qty ...))])
                                     (and (eq? (syntax-e a2) 'buy)
                                          (eq? (syntax-e t2) 'call)
                                          (>= (syntax-e q2) 1))))))))
                   "Naked short calls or puts are not allowed in safe mode"
     
     #'(define strategy-name
         (hash 'ticker 'ticker-value
               'current-price cp
               'safe-mode safe
               'legs (list (list 'action-sym qty 'type-sym s 'expiration exp) ...)))]))

;; Example that would error
(define-option-strategy risky-strat
  #:ticker 'TSLA
  #:current-price 250.50
  #:safe-mode #t
  (buy 1 call #:strike 300 #:expiration 30)  ;; Naked call
  (sell 1 call  #:strike 200 #:expiration 30)) ;; Naked put

;; Valid covered strategy
(define-option-strategy safe-strat
  #:ticker 'GOOG
  #:current-price 145.75
  #:safe-mode #t
  (sell 1 call #:strike 150 #:expiration 30)
  (buy  1 call #:strike 150 #:expiration 30)  ;; Covers call
  (sell 1 put  #:strike 140 #:expiration 30)
  (buy  1 put  #:strike 140 #:expiration 30)) ;; Covers put



;; Example usage
#|
(define-option-strategy AAPL-strat
  #:ticker 'AAPL
  #:current-price 182.52
  #:safe-mode #t
  (sell  1 call  #:strike 200 #:expiration 20))
|#

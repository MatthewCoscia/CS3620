#lang racket

(require (for-syntax racket/syntax racket/list))


;; This macro defines an options trading strategy with a payoff function
;; and automatically calculates breakeven points. It also enforces 
;; compile-time risk constraints, right now I only have preventing naked short positions.
;; but we can implement more constraints to keep the user on the right track
(define-for-syntax (accumulate-risks legs)
  (define (iter legs call-buy call-sell put-buy put-sell)
    (if (null? legs)
        (list call-buy call-sell put-buy put-sell)
        (syntax-case (car legs) (buy sell)
          [(buy qty type #:strike strike)
           (if (equal? (syntax-e #'type) 'call)
               (iter (cdr legs) (+ call-buy (syntax-e #'qty)) call-sell put-buy put-sell)
               (iter (cdr legs) call-buy call-sell (+ put-buy (syntax-e #'qty)) put-sell))]
          
          [(sell qty type #:strike strike)
           (if (equal? (syntax-e #'type) 'call)
               (iter (cdr legs) call-buy (+ call-sell (syntax-e #'qty)) put-buy put-sell)
               (iter (cdr legs) call-buy call-sell put-buy (+ put-sell (syntax-e #'qty))))]
          
          [else (iter (cdr legs) call-buy call-sell put-buy put-sell)])))
  (iter legs 0 0 0 0))

(define-syntax (define-option-strategy-with-breakeven stx)
  (syntax-case stx (buy sell)
    [(_ name leg ...)
     (let* ([legs (syntax->list #'(leg ...))]
            [risks (accumulate-risks legs)]
            [call-buy (list-ref risks 0)]
            [call-sell (list-ref risks 1)]
            [put-buy (list-ref risks 2)]
            [put-sell (list-ref risks 3)])
       
       (when (or (> call-sell call-buy) (> put-sell put-buy))
         (raise-syntax-error 'define-option-strategy-with-breakeven
                             "Risk constraint violated: naked position not allowed" stx))
       
       (with-syntax ([breakeven-name (format-id #'name "~a-breakeven" (syntax-e #'name))])
         #`(begin
             (define (name S)
               (+ #,@(map (lambda (leg)
                            (syntax-case leg (buy sell)
                              [(buy qty type #:strike strike)
                               (if (equal? (syntax-e #'type) 'call)
                                   #'(* qty (max (- S strike) 0))
                                   #'(* qty (max (- strike S) 0)))]
                              
                              [(sell qty type #:strike strike)
                               (if (equal? (syntax-e #'type) 'call)
                                   #'(- (* qty (max (- S strike) 0)))
                                   #'(- (* qty (max (- strike S) 0))))]))
                          legs)))
             
             (define (breakeven-name)
               (let loop ([S 50.0] [breakevens '()])
                 (cond
                   [(> S 150.0) (reverse breakevens)]
                   [else
                    (loop (+ S 0.1)
                          (if (< (abs (name S)) 1e-6)
                              (cons S breakevens)
                              breakevens))]))))))]))

;; ✅ Valid strategy
(define-option-strategy-with-breakeven iron-condor
  (buy 1 put #:strike 90)
  (sell 1 put #:strike 95)
  (sell 1 call #:strike 105)
  (buy 1 call #:strike 110))

(displayln (iron-condor 105))      ; => 0
(displayln (iron-condor-breakeven)) ; => (95.0 105.0)

;; This strategy will raise a **compile-time error** due to a naked short position
#|
(define-option-strategy-with-breakeven risky-strategy
  (sell 1 call #:strike 100)) ; Naked call, compile-time error
|#
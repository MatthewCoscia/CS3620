#lang racket
(require plot)

(define-syntax-rule (define-option name strike-premium ...)
  (define name (lambda () (graph-strategy strike-premium ...))))

(define-syntax-rule (define-option-strategy name option ...)
  (define name (lambda () (graph-strategy-multi (list option ...)))))

(define (graph-strategy-multi options)
  (let* ([x-min (apply min (map car options))]
         [x-max (apply max (map car options))])
    (define (payoff x)
      (apply + (map (lambda (opt) ((graph-strategy opt) x)) options)))
    (plot (function payoff (- x-min 10) (+ x-max 10)) #:title "Option Strategy Payoff")))

(define (graph-strategy strike-premium)
  (let ([strike (first strike-premium)]
        [premium (second strike-premium)])
    (lambda (x) (- (max 0 (- x strike)) premium))))

;; Example Usage:
(define-option my-call '(100 1))
(my-call)
(define-option-strategy my-strategy '(100 5) '(110 2))
(my-strategy)

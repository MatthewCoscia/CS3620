#lang racket

;; Purpose of DSL

;; Options Trading DSL
;;   - Define easily
;;   - Visualize
;;   - Keep yourself out of debt
;;       - error checking
;;       - safe mode















;; Demo

;;   - 2d view
;;   - 3d view












;; Example inupt

;; Two Ways to define the same thing:
(define-option-strategy call-debit-spread
  #:ticker 'TSLA
  #:ticker-price 250.50
  #:safe-mode #t
  (buy 10 call #:strike 250 #:expiration 3) 
  (sell 10 call  #:strike 255 #:expiration 3))


(define-option-strategy-shortcuts call-debit-spread-shortened
  #:ticker 'TSLA
  #:ticker-price 250.50
  #:safe-mode #t
  #:quantity 10
  (call-debit-spread 250 255 3))


(define-option-strategy collar-spread
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  ((buy ,qty-stx shares)
   buy ,qty-stx  put  #:strike ,long-put-strike  #:expiration ,expiration)
  (sell ,qty-stx call #:strike ,short-call-strike #:expiration ,expiration)

(define-option-strategy-shortcuts collar-shortened
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  (collar 140 150 7))








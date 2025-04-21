#lang racket
(require math/special-functions
         plot)
(require "functions.rkt")

(define (make-single-strategy-plot strategy label color x-min x-max
                                   #:days-since-purchase [days-since #f])
  (define (payoff x)
    (let* ([expiration
            (apply min
                   (map option-leg-expiration
                        (filter option-leg? (strategy-legs strategy))))] 
           [clipped-days (or days-since expiration)])
      (total-strategy-value-at-time strategy x clipped-days)))

  (define breakevens (find-breakeven-function payoff x-min x-max 0.001))
  (list (function payoff #:label label #:color color)
        (points (map (lambda (x) (vector x (payoff x))) breakevens)
                #:sym 'circle #:size 8 #:color color #:label #f)))


(define (render-multiple-strategies-3d strategies min-price max-price 
                                     min-days max-days price-step day-step)
  (let-values ([(x-min x-max)
                (get-plot-bounds strategies min-price max-price)])
    (parameterize ([plot-new-window? #t])
      (plot3d
       (append
        (for/list ([strat-info strategies])
          (match-define (list strategy label color) strat-info)
          (surface3d
           (λ (x y)
             (total-strategy-value-at-time strategy x y))
           x-min x-max
           min-days max-days
           #:color color)))
       #:title "Option Strategy Value Over Time (3D)"
       #:x-label "Stock Price"
       #:y-label "Days Since Purchase"
       #:z-label "Profit/Loss"
       #:x-min x-min
       #:x-max x-max
       #:y-min min-days
       #:y-max max-days
       #:width 1400
       #:height 600))))

(define (render-multiple-strategies-2d strategies min-price max-price days-since)
  (let-values ([(x-min x-max)
                (get-plot-bounds strategies min-price max-price)])
    (parameterize ([plot-new-window? #t])  
      (plot (append-map (lambda (strat-info)
                          (match-define
                            (list strategy label color) strat-info)
                          (make-single-strategy-plot
                           strategy label color x-min
                           x-max #:days-since-purchase days-since))
                        strategies)
            #:title "Option Strategy Comparison"
            #:x-label "Stock Price"
            #:y-label "Profit/Loss"
            #:x-min x-min
            #:x-max x-max
            #:width 1400
            #:height 600
            #:legend-anchor 'outside-right))))

(define (render-decision strategy-triplets use-3d? min-price max-price 
                       min-days max-days price-step day-step days-since)
  (define (get-expiration-max strat)
    (apply max (map option-leg-expiration (filter option-leg?
                                                  (strategy-legs strat)))))

  (define computed-max-days
    (or max-days
        (apply max
               (map (lambda (triplet)
                      (define strat (first triplet))
                      (get-expiration-max strat))
                    strategy-triplets))))

  (if use-3d?
      (render-multiple-strategies-3d
       strategy-triplets
       min-price max-price min-days computed-max-days price-step day-step)
      (render-multiple-strategies-2d
       strategy-triplets min-price max-price days-since)))

(provide
  make-single-strategy-plot
  render-multiple-strategies-3d
  render-multiple-strategies-2d
  render-decision)

(module+ test
  
  (printf "Skipping GUI tests that require display\n")

  (void)) ;; Return void to avoid test failures
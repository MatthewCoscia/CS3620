#lang racket

(require (for-syntax racket/base
                     racket/match 
                     racket/syntax
                     syntax/parse)
         racket/date
         plot
         rackunit
         rackunit/text-ui
         math/special-functions)


(struct strategy (name ticker ticker-price safe-mode volatility
                      risk-free-rate legs)
  #:transparent)
(struct option-leg (action qty type strike expiration premium)
  #:transparent)
(struct shares-leg (action qty)
  #:transparent)


(begin-for-syntax
  (define-syntax-class action
    #:description "buy or sell action"
    (pattern act:id
             #:when (memq (syntax->datum #'act) '(buy sell))))
  
  (define-syntax-class positive-whole-qty
    #:description "positive whole number quantity"
    (pattern n:expr
             #:when (or (not (number? (syntax->datum #'n)))
                        (and (integer? (syntax->datum #'n))
                             (positive? (syntax->datum #'n))))))
  
  (define-syntax-class option-type
    #:description "call or put option type"
    (pattern t:id
             #:when (memq (syntax->datum #'t) '(call put))))
  
  (define-syntax-class non-negative-premium
    #:description "non-negative premium value"
    (pattern n:expr
             #:when (or (not (number? (syntax->datum #'n)))
                        (and (number? (syntax->datum #'n))
                             (not (negative? (syntax->datum #'n)))))))
  
  (define-syntax-class ticker-symbol
    #:description "stock ticker symbol"
    (pattern sym:expr))
  
  (define-syntax-class positive-price
    #:description "positive price value"
    (pattern n:expr
             #:when (or (syntax->datum #'n)
                        (and (number? (syntax->datum #'n))
                             (positive? (syntax->datum #'n))))))
  
  (define-syntax-class safe-mode
    #:description "safety mode boolean"
    (pattern b:expr))
   
  (define-syntax-class leg-pattern
    #:description "option or shares leg pattern"
    ;; Pattern for option legs
    (pattern (action:action
              qty:positive-whole-qty
              type:option-type
              #:strike s:expr
              #:expiration exp:expr
              (~optional (~seq #:premium p:non-negative-premium)
                         #:defaults ([p #'#f])))
             #:with result #'(option-leg 'action.act qty 'type.t s exp p))
    ;; Pattern for share legs 
    (pattern (action:action
              qty:positive-whole-qty
              shares-kw)
             #:when (equal? (syntax->datum #'shares-kw) 'shares)
             #:with result #'(shares-leg 'action.act qty))))

;; Define the main macro
(define-syntax (define-option-strategy stx)
  (syntax-parse stx
    [(_ strategy-name:id
        #:ticker ticker:ticker-symbol
        #:ticker-price cp:positive-price
        #:safe-mode safe:safe-mode
        (~optional (~seq #:volatility vol:expr)
                   #:defaults ([vol #'0.2]))    ;; Default Volatility: 20%
        (~optional (~seq #:risk-free-rate rfr:expr)
                   #:defaults ([rfr #'0.05]))   ;; Default Risk Free Rate: 5%
        legs:leg-pattern ...)
     
     #`(define strategy-name
         (strategy
          'strategy-name
          ticker.sym          ;; Ticker symbol
          cp                  ;; Ticker price
          safe                ;; Safe mode?
          vol                 ;; Volatility
          rfr                 ;; Risk-free rate
          (list legs.result ...)))]))


#|
Supported Strategies
--------------------

Strategy                | Required Arguments                                         | Legs Generated
------------------------|------------------------------------------------------------|-----------------------------------------------------------
call-debit-spread       | (buy-strike sell-strike expiration)                        | Buy call @ strike1, Sell call @ strike2
put-debit-spread        | (buy-strike sell-strike expiration)                        | Buy put @ strike1, Sell put @ strike2
butterfly-spread        | (low-strike mid-strike high-strike expiration)             | Buy call @ k1, Sell 2 calls @ k2, Buy call @ k3
call-credit-spread      | (sell-strike buy-strike expiration)                        | Sell call @ strike1, Buy call @ strike2
put-credit-spread       | (sell-strike1 buy-strike expiration)                       | Sell put @ strike1, Buy put @ strike2
iron-condor             | (strike1-g1 strike2-g1 strike3-g2 strike4-g2 expiration)   | Buy put @ k1, Sell put @ k2, Sell call @ k3, Buy call @ k4
iron-butterfly          | (low-strike mid-strike high-strike expiration)             | Buy put @ k1, Sell put @ k2, Sell call @ k2, Buy call @ k3
long-straddle           | (strike expiration)                                        | Buy call + put @ same strike
long-strangle           | (put-strike call-strike expiration)                        | Buy put @ put-strike, Buy call @ call-strike
covered-call            | (strike expiration)                                        | Buy 100 shares, Sell 1 call
collar                  | (long-put-strike short-call-strike expiration)             | Buy 100 shares, Buy 1 put, Sell 1 call
diagonal-call-spread    | (near-strike near-expiration far-strike far-expiration)    | Sell near call, Buy far call
|#

;; Validate strategy inputs before expansion
(define-for-syntax (validate-strategy-args strategy args-list)
  (define (strike<? a b)
    (and (number? a) (number? b) (< a b)))

  (define (strike>? a b)
    (and (number? a) (number? b) (> a b)))

  (case strategy
    [(call-debit-spread)
     (define-values (buy-strike sell-strike expiration) (apply values args-list))
     (unless (strike<? buy-strike sell-strike)
       (error "Invalid call-debit-spread: buy strike must be less than sell strike"))]

    [(put-debit-spread)
     (define-values (buy-strike sell-strike expiration) (apply values args-list))
     (unless (strike>? buy-strike sell-strike)
       (error "Invalid put-debit-spread: buy strike must be greater than sell strike"))]

    [(call-credit-spread)
     (define-values (sell-strike buy-strike expiration) (apply values args-list))
     (unless (strike>? sell-strike buy-strike)
       (error "Invalid call-credit-spread: sell strike must be greater than buy strike"))]

    [(put-credit-spread)
     (define-values (sell-strike buy-strike expiration) (apply values args-list))
     (unless (strike<? sell-strike buy-strike)
       (error "Invalid put-credit-spread: sell strike must be less than buy strike"))]

    [(butterfly-spread iron-butterfly)
     (define-values (k1 k2 k3 expiration) (apply values args-list))
     (unless (and (not (= k1 k2)) (not (= k2 k3)) (not (= k1 k3)))
       (error "Butterfly: strikes must be distinct"))
     (unless (and (strike<? k1 k2) (strike<? k2 k3))
       (error (format "Invalid ~a: strikes must be in ascending order (k1 < k2 < k3)" strategy)))]

    [(iron-condor)
     (define-values (k1 k2 k3 k4 expiration) (apply values args-list))
     (unless (and (strike<? k1 k2) (strike<? k2 k3) (strike<? k3 k4))
       (error "Invalid iron-condor: strikes must be in ascending order (k1 < k2 < k3 < k4)"))]

    [(long-strangle)
     (define-values (put-strike call-strike expiration) (apply values args-list))
     (unless (strike<? put-strike call-strike)
       (error "Invalid long-strangle: put strike must be less than call strike"))]

    [else (void)]))

(define-for-syntax (expand-strategy-legs strategy args-list qty-stx)
  (validate-strategy-args strategy args-list)
  (define qty*100-stx #`(* #,qty-stx 100))
  (define qty*2-stx #`(* #,qty-stx 2))
  (cond
    ;; Call Debit Spread
    [(eq? strategy 'call-debit-spread)
     (define-values (strike1 strike2 expiration) (apply values args-list))
     (list
      `(buy ,qty-stx call #:strike ,strike1 #:expiration ,expiration)
      `(sell ,qty-stx call #:strike ,strike2 #:expiration ,expiration))]

    ;; Put Debit Spread
    [(eq? strategy 'put-debit-spread)
     (define-values (strike1 strike2 expiration) (apply values args-list))
     (list
      `(buy ,qty-stx put #:strike ,strike1 #:expiration ,expiration)
      `(sell ,qty-stx put #:strike ,strike2 #:expiration ,expiration))]

    ;; Butterfly spread
    [(eq? strategy 'butterfly-spread)
     (define-values (k1 k2 k3 expiration) (apply values args-list))
     (list
      `(buy ,qty-stx call #:strike ,k1 #:expiration ,expiration)
      `(sell qty*2-stx call #:strike ,k2 #:expiration ,expiration)
      `(buy ,qty-stx call #:strike ,k3 #:expiration ,expiration))]

    ;; Call credit spread
    [(eq? strategy 'call-credit-spread)
     (define-values (strike1 strike2 expiration) (apply values args-list))
     (list
      `(sell ,qty-stx call #:strike ,strike1 #:expiration ,expiration)
      `(buy ,qty-stx call #:strike ,strike2 #:expiration ,expiration))]

    ;; Put credit spread
    [(eq? strategy 'put-credit-spread)
     (define-values (strike1 strike2 expiration) (apply values args-list))
     (list
      `(sell ,qty-stx put #:strike ,strike1 #:expiration ,expiration)
      `(buy ,qty-stx put #:strike ,strike2 #:expiration ,expiration))]

    ;; Iron condor
    [(eq? strategy 'iron-condor)
     (define-values (k1 k2 k3 k4 expiration) (apply values args-list))
     (list
      `(buy ,qty-stx put  #:strike ,k1 #:expiration ,expiration)
      `(sell ,qty-stx put #:strike ,k2 #:expiration ,expiration)
      `(sell ,qty-stx call #:strike ,k3 #:expiration ,expiration)
      `(buy ,qty-stx call #:strike ,k4 #:expiration ,expiration))]

    ;; Iron butterfly
    [(eq? strategy 'iron-butterfly)
     (define-values (k1 k2 k3 expiration) (apply values args-list))
     (list
      `(buy ,qty-stx put  #:strike ,k1 #:expiration ,expiration)
      `(sell ,qty-stx put  #:strike ,k2 #:expiration ,expiration)
      `(sell ,qty-stx call #:strike ,k2 #:expiration ,expiration)
      `(buy ,qty-stx call #:strike ,k3 #:expiration ,expiration))]

    ;; Long straddle
    [(eq? strategy 'long-straddle)
     (define-values (strike expiration) (apply values args-list))
     (list
      `(buy ,qty-stx call #:strike ,strike #:expiration ,expiration)
      `(buy ,qty-stx put  #:strike ,strike #:expiration ,expiration))]

    ;; Long strangle
    [(eq? strategy 'long-strangle)
     (define-values (put-strike call-strike expiration) (apply values args-list))
     (list
      `(buy ,qty-stx put  #:strike ,put-strike  #:expiration ,expiration)
      `(buy ,qty-stx call #:strike ,call-strike #:expiration ,expiration))]

    ;; Covered call
    [(eq? strategy 'covered-call)
     (define-values (strike expiration) (apply values args-list))
     (list
      '(buy qty*100-stx shares)
      `(sell ,qty-stx call #:strike ,strike #:expiration ,expiration))]

    ;; Collar
    [(eq? strategy 'collar)
     (define-values (long-put-strike short-call-strike expiration) (apply values args-list))
     (list
      `(buy ,qty*100-stx shares)
      `(buy ,qty-stx  put  #:strike ,long-put-strike  #:expiration ,expiration)
      `(sell ,qty-stx call #:strike ,short-call-strike #:expiration ,expiration))]

    ;; Diagonal call spread
    [(eq? strategy 'diagonal-call-spread)
     (define-values (near-strike near-expiration far-strike far-expiration)
       (apply values args-list))
     (list
      `(buy ,qty-stx call #:strike ,far-strike  #:expiration ,far-expiration)
      `(sell ,qty-stx call #:strike ,near-strike #:expiration ,near-expiration))]

        [else
     (error "Unknown strategy:" strategy)]))

(define-syntax (define-option-strategy-shortcuts stx)
  (syntax-parse stx
    [(_ strategy-name:id
        #:ticker ticker:expr
        #:ticker-price cp:expr
        #:safe-mode safe:expr
        (~optional [~seq #:volatility vol:expr] #:defaults ([vol #'0.2]))
        (~optional [~seq #:risk-free-rate rfr:expr] #:defaults ([rfr #'0.05]))
        (~optional [~seq #:quantity q:expr] #:defaults ([q #'1]))
        (strategy-type:id args:expr ...))


     (define strategy (syntax-e #'strategy-type))
     (define args-list (map syntax-e (syntax->list #'(args ...))))
  
     (define legs (expand-strategy-legs strategy args-list #'q))

     #`(define-option-strategy strategy-name
         #:ticker         ticker
         #:ticker-price   cp
         #:safe-mode      safe
         #:volatility     vol
         #:risk-free-rate rfr
         #,@legs)]))


(define (share-payoff stock-price action quantity ticker-price)
  (let ([price-change (- stock-price ticker-price)])
    (* quantity 
       (if (eq? action 'buy)
           price-change   ; Long position: profit when price increases
           (- price-change)))))  ; Short position: profit when price decreases

(define (total-strategy-payoff strat stock-price)
  (let* ([legs (strategy-legs strat)]
         [ticker-price (strategy-ticker-price strat)]
         [volatility (strategy-volatility strat)]
         [risk-free-rate (strategy-risk-free-rate strat)])
    (apply +
           (map (λ (leg)
                  (cond
                    [(option-leg? leg)
                     (option-payoff stock-price
                                    (option-leg-strike leg)
                                    (option-leg-action leg)
                                    (option-leg-type leg)
                                    (option-leg-qty leg)
                                    (option-leg-premium leg)
                                    risk-free-rate
                                    volatility
                                    (option-leg-expiration leg)
                                    ticker-price)]
                    [(shares-leg? leg)
                     (share-payoff stock-price
                                  (shares-leg-action leg)
                                  (shares-leg-qty leg)
                                  ticker-price)]
                    [else (error "Unknown leg type")]))
                legs))))

(define (option-payoff stock-price strike action type quantity premium 
                       risk-free-rate volatility expiration-days ticker-price)
  (let* ([T (/ expiration-days 365.0)]  ;; Convert days to years
         [actual-premium (if (equal? premium #f)
                             (calculate-premium strike ticker-price T 
                                                risk-free-rate volatility type)
                             premium)]  ;; Use either Black-Scholes
         ;; or manual premium
         [raw-payoff (cond
                       [(eq? type 'call) (max 0 (- stock-price strike))]
                       [(eq? type 'put)  (max 0 (- strike stock-price))])]
         [intrinsic-value (* quantity raw-payoff)]  ;; Pure option value
         [initial-cost (* quantity actual-premium)])  ;; Cost upfront
    
    ;; Adjust for long and short call behavior
    (if (eq? action 'buy)
        (- intrinsic-value initial-cost)  ;; Buying: Pay premium upfront
        (- initial-cost intrinsic-value))))  ;; Limit gains beyond strike





(define (find-breakeven strategy min-price max-price step)
  (let loop ([price min-price] [breakevens '()])
    (if (> price max-price)
        (reverse breakevens)
        (let ([profit (total-strategy-payoff strategy price)]
              [next-profit (total-strategy-payoff strategy (+ price step))])
          (if (or (and (>= profit 0) (< next-profit 0))
                  (and (<= profit 0) (> next-profit 0)))
              (loop (+ price step) (cons price breakevens))
              (loop (+ price step) breakevens))))))


(define (cdf x)
  (/ (+ 1 (erf (/ x (sqrt 2)))) 2))

(define (black-scholes-call S K T r sigma)
  (if (= sigma 0)
      (let ([discounted-K (* K (exp (* (- r) T)))])
        (max 0 (- S discounted-K)))
      (let* ([d1 (/ (+ (log (/ S K)) (* (+ r (/ (expt sigma 2) 2)) T))
                    (* sigma (sqrt T)))]
             [d2 (- d1 (* sigma (sqrt T)))]
             [Nd1 (cdf d1)]
             [Nd2 (cdf d2)]
             [discount-factor (exp (* (- r) T))])
        (- (* S Nd1) (* K discount-factor Nd2)))))

(define (black-scholes-put S K T r sigma)
  (if (= sigma 0)
      (let ([discounted-K (* K (exp (* (- r) T)))])
        (max 0 (- discounted-K S)))
      (let* ([d1 (/ (+ (log (/ S K)) (* (+ r (/ (expt sigma 2) 2)) T))
                    (* sigma (sqrt T)))]
             [d2 (- d1 (* sigma (sqrt T)))]
             [Nd1 (cdf (- d1))]
             [Nd2 (cdf (- d2))]
             [discount-factor (exp (* (- r) T))])
        (- (* K discount-factor Nd2) (* S Nd1)))))



(define (calculate-premium strike price expiration
                           risk-free-rate volatility type)
  (let ([premium (if (eq? type 'call)
                     (black-scholes-call price strike
                                         expiration risk-free-rate volatility)
                     (black-scholes-put price
                                        strike
                                        expiration
                                        risk-free-rate volatility))])
    premium))


(define (print-strategy strat)
  (define min-price (- (strategy-ticker-price strat) 50))
  (define max-price (+ (strategy-ticker-price strat) 50))
  (define step 1)
  (for ([price (in-range min-price (+ max-price step) step)])
    (printf "Stock Price: ~a | Payoff: ~a\n"
            price
            (total-strategy-payoff strat price))))





;; Calculate plot boundaries
(define (get-plot-bounds strategies min-price max-price)
  (if (or min-price max-price)
      (values (or min-price
                  (- (strategy-ticker-price (caar strategies)) 50))
              (or max-price
                  (+ (strategy-ticker-price (caar strategies)) 50)))
      (let ([ticker-prices (map (λ (s) (strategy-ticker-price (car s)))
                                 strategies)])
        (values (- (apply min ticker-prices) 50)
                (+ (apply max ticker-prices) 50)))))


;;  Create plot elements for a single strategy
(define (make-single-strategy-plot strategy label color x-min x-max)
  (define (payoff x) (total-strategy-payoff strategy x))
  (define breakevens (find-breakeven strategy x-min x-max 1))
  (list (function payoff #:label label #:color color)
        (points (map (λ (x) (vector x (payoff x))) breakevens)
                #:sym 'circle #:size 8 #:color color #:label #f)))

;; Main function
(define (graph-multiple-strategies strategies
                                   #:min-price [min-price #f]
                                   #:max-price [max-price #f])
  (let-values ([(x-min x-max)
                (get-plot-bounds strategies min-price max-price)])
    (parameterize ([plot-new-window? #t])  
      (plot (append-map (λ (strat-info)
                          (match-define
                            (list strategy label color) strat-info)
                          (make-single-strategy-plot
                           strategy label color x-min x-max))
                        strategies)
            #:title "Option Strategy Comparison"
            #:x-label "Stock Price"
            #:y-label "Profit/Loss"
            #:x-min x-min
            #:x-max x-max
            #:width 1400
            #:height 600
            #:legend-anchor 'outside-right))))



(define-option-strategy-shortcuts bullish-strat-shortened
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  (call-debit-spread 140 150 7))

(define-option-strategy-shortcuts collar-shortened
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  (collar 140 150 7))

(define (graph-preview)
  (graph-multiple-strategies
   (list (list collar-shortened "Bull Call Spread" "blue"))
   #:min-price 50  
   #:max-price 350))




(define (graph-preview-single)
  (graph-multiple-strategies
   (list (list bullish-strat-shortened "Bull Call Spread" "blue"))))


(graph-preview)
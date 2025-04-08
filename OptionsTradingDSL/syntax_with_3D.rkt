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

;; Define the structures
(struct strategy (name ticker ticker-price safe-mode volatility
                      risk-free-rate legs)
  #:transparent)
(struct option-leg (action qty type strike expiration premium)
  #:transparent)
(struct shares-leg (action qty)
  #:transparent)

;; Define the syntax classes at compile time
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
    ;; Pattern for share legs - correctly match the 'shares' symbol
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
                   #:defaults ([rfr #'0.05]))   ;; Default Risk-Free Rate: 5%'
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

(define-for-syntax (expand-strategy-legs strategy args-list)
  (cond
    [(eq? strategy 'call-debit-spread)
     (define-values (strike1 strike2 expiration) (apply values args-list))
     (list
      `(buy 1 call #:strike ,strike1 #:expiration ,expiration)
      `(sell 1 call #:strike ,strike2 #:expiration ,expiration))]

    [(eq? strategy 'put-debit-spread)
     (define-values (strike1 strike2 expiration) (apply values args-list))
     (list
      `(buy 1 put #:strike ,strike1 #:expiration ,expiration)
      `(sell 1 put #:strike ,strike2 #:expiration ,expiration))]

    [(eq? strategy 'butterfly-spread)
     (define-values (k1 k2 k3 expiration) (apply values args-list))
     (list
      `(buy 1 call #:strike ,k1 #:expiration ,expiration)
      `(sell 2 call #:strike ,k2 #:expiration ,expiration)
      `(buy 1 call #:strike ,k3 #:expiration ,expiration))]

    ;; Call credit spread
    [(eq? strategy 'call-credit-spread)
     (define-values (strike1 strike2 expiration) (apply values args-list))
     (list
      `(sell 1 call #:strike ,strike1 #:expiration ,expiration)
      `(buy 1 call #:strike ,strike2 #:expiration ,expiration))]

    ;; Put credit spread
    [(eq? strategy 'put-credit-spread)
     (define-values (strike1 strike2 expiration) (apply values args-list))
     (list
      `(sell 1 put #:strike ,strike1 #:expiration ,expiration)
      `(buy 1 put #:strike ,strike2 #:expiration ,expiration))]

    ;; Iron condor
    [(eq? strategy 'iron-condor)
     (define-values (k1 k2 k3 k4 expiration) (apply values args-list))
     (list
      `(buy 1 put  #:strike ,k1 #:expiration ,expiration)
      `(sell 1 put #:strike ,k2 #:expiration ,expiration)
      `(sell 1 call #:strike ,k3 #:expiration ,expiration)
      `(buy 1 call #:strike ,k4 #:expiration ,expiration))]

    ;; Iron butterfly
    [(eq? strategy 'iron-butterfly)
     (define-values (k1 k2 k3 expiration) (apply values args-list))
     (list
      `(buy 1 put  #:strike ,k1 #:expiration ,expiration)
      `(sell 1 put  #:strike ,k2 #:expiration ,expiration)
      `(sell 1 call #:strike ,k2 #:expiration ,expiration)
      `(buy 1 call #:strike ,k3 #:expiration ,expiration))]

    ;; Long straddle
    [(eq? strategy 'long-straddle)
     (define-values (strike expiration) (apply values args-list))
     (list
      `(buy 1 call #:strike ,strike #:expiration ,expiration)
      `(buy 1 put  #:strike ,strike #:expiration ,expiration))]

    ;; Long strangle
    [(eq? strategy 'long-strangle)
     (define-values (put-strike call-strike expiration) (apply values args-list))
     (list
      `(buy 1 put  #:strike ,put-strike  #:expiration ,expiration)
      `(buy 1 call #:strike ,call-strike #:expiration ,expiration))]

    ;; Covered call
    [(eq? strategy 'covered-call)
     (define-values (strike expiration) (apply values args-list))
     (list
      '(buy 100 shares)
      `(sell 1 call #:strike ,strike #:expiration ,expiration))]

    ;; Collar
    [(eq? strategy 'collar)
     (define-values (long-put-strike short-call-strike expiration) (apply values args-list))
     (list
      '(buy 100 shares)
      `(buy 1  put  #:strike ,long-put-strike  #:expiration ,expiration)
      `(sell 1 call #:strike ,short-call-strike #:expiration ,expiration))]

    ;; Diagonal call spread (example)
    [(eq? strategy 'diagonal-call-spread)
     (define-values (near-strike near-expiration far-strike far-expiration)
       (apply values args-list))
     (list
      `(buy 1 call #:strike ,far-strike  #:expiration ,far-expiration)
      `(sell 1 call #:strike ,near-strike #:expiration ,near-expiration))]

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
        (strategy-type:id args:expr ...))

     (define strategy (syntax-e #'strategy-type))
     (define args-list (map syntax-e (syntax->list #'(args ...))))

     (define legs (expand-strategy-legs strategy args-list))

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

(define (strategy-max-expiration strat)
  (apply max
         (for/list ([leg (strategy-legs strat)]
                    #:when (option-leg? leg))
           (option-leg-expiration leg))))



(define (option-value-at-time stock-price
                              strike
                              action
                              type
                              quantity
                              premium
                              risk-free-rate
                              volatility
                              days-since-purchase
                              original-expiration-days
                              ticker-price)
  (let* ([T (/ (- original-expiration-days days-since-purchase) 365.0)] ; flip it!
         [original-T (/ original-expiration-days 365.0)]
         [actual-premium
          (if (equal? premium #f)
              (calculate-premium strike
                                 ticker-price
                                 original-T
                                 risk-free-rate
                                 volatility
                                 type)
              premium)]
         [bs-value
          (calculate-premium strike
                             stock-price
                             T
                             risk-free-rate
                             volatility
                             type)]
         [net-value (* quantity bs-value)]
         [cost      (* quantity actual-premium)])
    (if (eq? action 'buy)
        (- net-value cost)
        (- cost net-value)))) ;; Selling the option means net gain is cost - BS-value

(define (total-strategy-value-at-time strat stock-price days-remaining)
  (let* ([legs            (strategy-legs strat)]
         [ticker-price    (strategy-ticker-price strat)]
         [volatility      (strategy-volatility strat)]
         [risk-free-rate  (strategy-risk-free-rate strat)])
    (apply +
           (map
            (λ (leg)
              (cond
                [(option-leg? leg)
                 (option-value-at-time stock-price
                                       (option-leg-strike leg)
                                       (option-leg-action leg)
                                       (option-leg-type leg)
                                       (option-leg-qty leg)
                                       (option-leg-premium leg)
                                       risk-free-rate
                                       volatility
                                       days-remaining
                                       (option-leg-expiration leg)  ; << new arg!
                                       ticker-price)]
                [(shares-leg? leg)
                 ;; Shares payoff is basically stock-price - cost basis:
                 (share-payoff stock-price
                               (shares-leg-action leg)
                               (shares-leg-qty leg)
                               ticker-price)]
                [else
                 (error "Unknown leg type" leg)]))
            legs))))




;; Helper 1: Calculate plot boundaries
(define (get-plot-bounds strategies min-price max-price)
  (if (or min-price max-price)
      (let* ([base-price (strategy-ticker-price (caar strategies))]
             [offset (* base-price 0.5)])
        (values (or min-price (- base-price offset))
                (or max-price (+ base-price offset))))
      (let* ([ticker-prices (map (λ (s) (strategy-ticker-price (car s)))
                                 strategies)]
             [min-price-val (apply min ticker-prices)]
             [max-price-val (apply max ticker-prices)]
             [low-offset (* min-price-val 0.5)]
             [high-offset (* max-price-val 0.5)])
        (values (- min-price-val low-offset)
                (+ max-price-val high-offset)))))


;; Helper 2: Create plot elements for a single strategy
(define (make-single-strategy-plot strategy label color x-min x-max)
  (define (payoff x) (total-strategy-payoff strategy x))
  (define breakevens (find-breakeven strategy x-min x-max 1))
  (list (function payoff #:label label #:color color)
        (points (map (λ (x) (vector x (payoff x))) breakevens)
                #:sym 'circle #:size 8 #:color color #:label #f)))


(define (graph-decision strategy-triplets
                        #:3d [use-3d? #f]
                        #:min-price [min-price #f]
                        #:max-price [max-price #f]
                        #:min-days [min-days 1]
                        #:max-days [max-days #f]
                        #:price-step [price-step 5]
                        #:day-step [day-step 2])
  (define (get-expiration-max strat)
    (apply max (map option-leg-expiration (filter option-leg? (strategy-legs strat)))))

  (define computed-max-days
    (or max-days
        (apply max
               (map (λ (triplet)
                      (define strat (first triplet))
                      (get-expiration-max strat))
                    strategy-triplets))))

  (if use-3d?
      (graph-multiple-strategies-3d
       strategy-triplets
       #:min-price min-price
       #:max-price max-price
       #:min-days min-days
       #:max-days computed-max-days
       #:price-step price-step
       #:day-step day-step)
      (graph-multiple-strategies
       strategy-triplets
       #:min-price min-price
       #:max-price max-price)))


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

(define (graph-multiple-strategies-3d strategies
                                      #:min-price [min-price #f]
                                      #:max-price [max-price #f]
                                      #:min-days [min-days 1]
                                      #:max-days [max-days 60]
                                      #:price-step [price-step 5]
                                      #:day-step [day-step 2])
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
           min-days max-days)))
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
   (list (list collar-shortened "Bull Call Spread" "blue")
         (list bullish-strat-shortened "Bullish Short" "red"))
   #:min-price 50  ; Optional manual price range
   #:max-price 350))

(define (graph-preview-single)
  (graph-multiple-strategies
   (list (list bullish-strat-shortened "Bull Call Spread" "blue"))))



(define-option-strategy fancy-custom-strat
  #:ticker 'TSLA
  #:ticker-price 250
  #:safe-mode #f
  ;; Turn on 3D for fun
  (buy 1 call #:strike 240 #:expiration 30)
  (sell 1 call #:strike 260 #:expiration 30)
  (buy 1 put  #:strike 230 #:expiration 30))


(define-option-strategy big-curve
  #:ticker 'FAKE
  #:ticker-price 100
  #:safe-mode #f
  #:volatility 5.0          ; extremely high IV
  #:risk-free-rate 0.0
  (buy 1 call #:strike 100 #:expiration 60))



(define (test20)
(graph-decision big-curve
                #:3d #f
                #:label "High Vol ATM Call"
                #:color "purple"))

(define (test21)
(graph-decision big-curve
                #:3d #t
                #:label "High Vol ATM Call"
                #:color "purple"))

(define-option-strategy high-vol-straddle
  #:ticker 'TSLA
  #:ticker-price 250
  #:safe-mode #f
  #:volatility 1.2
  #:risk-free-rate 0.01
  (buy 1 call #:strike 250 #:expiration 30)
  (buy 1 put  #:strike 250 #:expiration 30))

(define-option-strategy high-vol-strangle
  #:ticker 'TSLA
  #:ticker-price 250
  #:safe-mode #f
  #:volatility 1.2
  #:risk-free-rate 0.01
  (buy 1 call #:strike 270 #:expiration 30)
  (buy 1 put  #:strike 230 #:expiration 30))

(define (high-vol-comparison-2d)
  (graph-multiple-strategies
   (list (list high-vol-straddle "Straddle" "green")
         (list high-vol-strangle "Strangle" "orange"))))

(define (high-vol-comparison-3d)
  (graph-multiple-strategies-3d
   (list (list high-vol-straddle "Straddle" "green")
         (list high-vol-strangle "Strangle" "orange"))))

(graph-decision
 (list (list high-vol-straddle "Straddle" "green")
       (list high-vol-strangle "Strangle" "orange"))
 #:3d #t
 #:min-price 200
 #:max-price 300)

(graph-decision
 (list (list high-vol-straddle "Straddle" "green")
       (list high-vol-strangle "Strangle" "orange"))
 #:3d #f)















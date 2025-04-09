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
    ;; Auto-detect shortcut syntax case (strategy-type with arguments)
    [(_ strategy-name:id
        #:ticker ticker:expr
        #:ticker-price cp:expr
        (~optional (~seq #:volatility vol:expr) #:defaults ([vol #'0.2]))
        (~optional (~seq #:risk-free-rate rfr:expr) #:defaults ([rfr #'0.05]))
        (~optional (~seq #:quantity q:expr) #:defaults ([q #'1]))
        (strategy-type:id args:expr ...))

     (let ()
       (define strategy (syntax-e #'strategy-type))
       (define args-list-stx (syntax->list #'(args ...)))
       (define args-list (syntax->datum #'(args ...)))
       (define legs (expand-strategy-legs
                     strategy args-list #'q args-list-stx))

       #`(define-option-strategy
           strategy-name
           #:ticker ticker
           #:ticker-price cp
           #:volatility vol
           #:risk-free-rate rfr
           #,@legs))]

    ;; Longform syntax case (explicit legs)
    [(_ strategy-name:id
        #:ticker ticker:expr
        #:ticker-price cp:expr
        (~optional (~seq #:volatility vol:expr) #:defaults ([vol #'0.2]))
        (~optional (~seq #:risk-free-rate rfr:expr) #:defaults ([rfr #'0.05]))
        legs:leg-pattern ...)

     #`(define strategy-name
         (strategy
          'strategy-name
          ticker
          cp
          #f  ;; Auto-insert default safe-mode value
          vol
          rfr
          (list legs.result ...)))]

    ;; Error case for invalid strategy syntax
    [_
     (raise-syntax-error 'define-option-strategy
                         "Invalid strategy syntax. Use either:\n"
                         stx)]))



;; Validate strategy inputs before expansion
(define-for-syntax (validate-strategy-args strategy args-list args-list-stx)
  (define (strike<? a b)
    (and (number? a) (number? b) (< a b)))

  (define (strike>? a b)
    (and (number? a) (number? b) (> a b)))

  (case strategy
    [(call-debit-spread)
     (define-values (buy-strike sell-strike expiration)
       (apply values args-list))
     (define-values (buy-stx sell-stx expiration-stx)
       (apply values args-list-stx))
     (unless (strike<? buy-strike sell-strike)
       (raise-syntax-error 'define-option-strategy
                           "Invalid call-debit-spread:
 buy strike must be less than sell strike"
                           buy-stx))]

    [(put-debit-spread)
     (define-values (buy-strike sell-strike expiration)
       (apply values args-list))
     (define-values (buy-stx sell-stx expiration-stx)
       (apply values args-list-stx))
     (unless (strike>? buy-strike sell-strike)
       (raise-syntax-error 'define-option-strategy
                           "Invalid put-debit-spread:
 buy strike must be greater than sell strike"
                           buy-stx))]

    [(call-credit-spread)
     (define-values (sell-strike buy-strike expiration)
       (apply values args-list))
     (define-values (sell-stx buy-stx expiration-stx)
       (apply values args-list-stx))
     (unless (strike>? sell-strike buy-strike)
       (raise-syntax-error 'define-option-strategy
                           "Invalid call-credit-spread: sell
strike must be greater than buy strike"
                           sell-stx))]


    [(put-credit-spread)
     (define-values (sell-strike buy-strike expiration)
       (apply values args-list))
     (define-values (sell-stx buy-stx expiration-stx)
       (apply values args-list-stx))
     (unless (strike<? sell-strike buy-strike)
       (raise-syntax-error 'define-option-strategy
                           "Invalid put-credit-spread:
sell strike must be less than buy strike"
                           sell-stx))]


    [(butterfly-spread iron-butterfly)
     (define-values (k1 k2 k3 expiration) (apply values args-list))
     (define-values (k1-stx k2-stx k3-stx expiration-stx)
       (apply values args-list-stx))
     (unless (and (not (= k1 k2)) (not (= k2 k3)) (not (= k1 k3)))
       (raise-syntax-error 'define-option-strategy
                           "Invalid butterfly: strikes must be distinct"
                           k1-stx))
     (unless (and (strike<? k1 k2) (strike<? k2 k3))
       (raise-syntax-error 'define-option-strategy
                           (format "Invalid ~a: strikes must
be in ascending order (k1 < k2 < k3)" strategy)
                           k1-stx))]


    [(iron-condor)
     (define-values (k1 k2 k3 k4 expiration) (apply values args-list))
     (define-values (k1-stx k2-stx k3-stx k4-stx expiration-stx)
       (apply values args-list-stx))
     (unless (and (strike<? k1 k2) (strike<? k2 k3) (strike<? k3 k4))
       (raise-syntax-error 'define-option-strategy
                           "Invalid iron-condor: strikes must be
 in ascending order (k1 < k2 < k3 < k4)"
                           k1-stx))]


    [(long-strangle)
     (define-values (put-strike call-strike expiration)
       (apply values args-list))
     (define-values (put-stx call-stx expiration-stx)
       (apply values args-list-stx))
     (unless (strike<? put-strike call-strike)
       (raise-syntax-error 'define-option-strategy
                           "Invalid long-strangle:
put strike must be less than call strike"
                           put-stx))]


    [else (void)]))

(define-for-syntax (expand-strategy-legs strategy args-list
                                         qty-stx args-list-stx)
  (validate-strategy-args strategy args-list args-list-stx)
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
     (define-values (put-strike call-strike expiration)
       (apply values args-list))
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
     (define-values (long-put-strike short-call-strike expiration)
       (apply values args-list))
     (list
      `(buy ,qty*100-stx shares)
      `(buy ,qty-stx  put  #:strike ,long-put-strike
            #:expiration ,expiration)
      `(sell ,qty-stx call #:strike ,short-call-strike
             #:expiration ,expiration))]

    ;; Diagonal call spread
    [(eq? strategy 'diagonal-call-spread)
     (define-values (near-strike near-expiration far-strike far-expiration)
       (apply values args-list))
     (list
      `(buy ,qty-stx call #:strike ,far-strike  #:expiration
            ,far-expiration)
      `(sell ,qty-stx call #:strike ,near-strike #:expiration
             ,near-expiration))]

        [else
     (error "Unknown strategy:" strategy)]))


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
                                                risk-free-rate
                                                volatility type)
                             premium)]  ;; Use either Black-Scholes
         ;; or manual premium
         [raw-payoff (cond
                       [(eq? type 'call) (max 0 (- stock-price strike))]
                       [(eq? type 'put)  (max 0 (- strike stock-price))])]
         [intrinsic-value (* quantity 100 raw-payoff)] ;; pure option value
         [initial-cost     (* quantity 100 actual-premium)])  ;; Cost upfront
    
    ;; Adjust for long and short call behavior
    (if (eq? action 'buy)
        (- intrinsic-value initial-cost)  ;; Buying: Pay premium upfront
        (- initial-cost intrinsic-value))))  ;; Limit gains beyond strike


(define (cdf x)
  (/ (+ 1 (erf (/ x (sqrt 2)))) 2))

(define (black-scholes-call S K T r sigma)
  (cond
    [(= T 0) (max 0 (- S K))]
    [(= sigma 0)
     (let ([discounted-K (* K (exp (* (- r) T)))])
       (max 0 (- S discounted-K)))]
    [else
     (let* ([d1 (/ (+ (log (/ S K)) (* (+ r (/ (expt sigma 2) 2)) T))
                   (* sigma (sqrt T)))]
            [d2 (- d1 (* sigma (sqrt T)))]
            [Nd1 (cdf d1)]
            [Nd2 (cdf d2)]
            [discount-factor (exp (* (- r) T))])
       (- (* S Nd1) (* K discount-factor Nd2)))]))


(define (black-scholes-put S K T r sigma)
  (cond
    [(= T 0) (max 0 (- K S))]
    [(= sigma 0)
     (let ([discounted-K (* K (exp (* (- r) T)))])
       (max 0 (- discounted-K S)))]
    [else
     (let* ([d1 (/ (+ (log (/ S K)) (* (+ r (/ (expt sigma 2) 2)) T))
                   (* sigma (sqrt T)))]
            [d2 (- d1 (* sigma (sqrt T)))]
            [Nd1 (cdf (- d1))]
            [Nd2 (cdf (- d2))]
            [discount-factor (exp (* (- r) T))])
       (- (* K discount-factor Nd2) (* S Nd1)))]))




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
  (let* ([T (/ (- original-expiration-days days-since-purchase) 365.0)]
         [cost-basis-per-contract
          (if (equal? premium #f)
             
              (calculate-premium strike ticker-price T
                                 risk-free-rate volatility type)
              ;; If premium *was* manually supplied, it's per contract already
              ;; so divide by 100 to keep it in contract terms
              premium)]
         [bs-value (calculate-premium strike stock-price T
                                      risk-free-rate volatility type)]
         [net-value (* quantity 100 bs-value)]
         [cost      (* quantity 100 cost-basis-per-contract)])
    (if (eq? action 'buy)
        (- net-value cost)
        (- cost net-value))))

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
                                       (option-leg-expiration leg)
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

;; Calculate plot boundaries
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


(define (find-breakeven-function payoff-fn min-price max-price step)
  (let loop ([price min-price] [breakevens '()])
    (if (> price max-price)
        (reverse breakevens)
        (let ([profit (payoff-fn price)]
              [next-profit (payoff-fn (+ price step))])
          (if (or (and (>= profit 0) (< next-profit 0))
                  (and (<= profit 0) (> next-profit 0)))
              (loop (+ price step) (cons price breakevens))
              (loop (+ price step) breakevens))))))


;;  Create plot elements for a single strategy
(define (make-single-strategy-plot strategy label color x-min x-max
                                   #:days-since-purchase [days-since #f])
  (define (payoff x)
    (if days-since
        (let* ([expiration
                (apply min
                       (map option-leg-expiration
                            (filter option-leg? (strategy-legs strategy))))]
               [clipped-days (min days-since expiration)])
          (total-strategy-value-at-time strategy x clipped-days))
        (total-strategy-payoff strategy x)))
  (define breakevens (find-breakeven-function payoff x-min x-max 0.001))
  (list (function payoff #:label label #:color color)
        (points (map (lambda (x) (vector x (payoff x))) breakevens)
                #:sym 'circle #:size 8 #:color color #:label #f)))

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
           min-days max-days
           #:color color))) ;; <- use the color!
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

;; Main function
(define (graph-multiple-strategies-2d strategies
                                      #:min-price [min-price #f]
                                      #:max-price [max-price #f]
                                      #:days-since-purchase [days-since #f])
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

(define (graph-decision strategy-triplets
                        #:3d [use-3d? #f]
                        #:min-price [min-price #f]
                        #:max-price [max-price #f]
                        #:min-days [min-days 1]
                        #:max-days [max-days #f]
                        #:price-step [price-step 5]
                        #:day-step [day-step 2]
                        #:days-since-purchase [days-since #f])
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
      (graph-multiple-strategies-3d
       strategy-triplets
       #:min-price min-price
       #:max-price max-price
       #:min-days min-days
       #:max-days computed-max-days
       #:price-step price-step
       #:day-step day-step)
      (graph-multiple-strategies-2d
       strategy-triplets
       #:min-price min-price
       #:max-price max-price
       #:days-since-purchase days-since)))



(define-option-strategy bullish-strat-shortened
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:quantity 2
  (call-debit-spread 140 150 7))

(define-option-strategy collar-shortened
  #:ticker 'AAPL
  #:ticker-price 145.75
  (collar 140 150 7))

(define (graph-preview)
  (graph-decision
   (list (list collar-shortened "Bull Call Spread" "blue"))
   #:3d #f
   #:min-price 50
   #:max-price 350))

(define (graph-preview-single)
  (graph-decision
   (list (list bullish-strat-shortened "Bull Call Spread" "blue"))
   #:3d #t))

(define-option-strategy decaying-call-spread
  #:ticker 'AAPL
  #:ticker-price 150
  #:volatility 0.3
  #:risk-free-rate 0.02
  (buy 1 call #:strike 145 #:expiration 1000)
  (sell 1 call #:strike 155 #:expiration 1000))


(define-option-strategy decaying-put-spread
  #:ticker 'AAPL
  #:ticker-price 150
  #:volatility 0.3
  #:risk-free-rate 0.02
  (buy 1 put #:strike 155 #:expiration 500)
  (sell 1 put #:strike 145 #:expiration 500))



(define (3dtest)
  (graph-decision
   (list (list decaying-call-spread "Call Debit Spread" "blue")
         (list decaying-put-spread "Put Debit Spread" "red"))
   #:3d #f
   #:days-since-purchase 500))


(define-option-strategy call-alone
  #:ticker 'AAPL
  #:ticker-price 150
  #:volatility 0.3
  #:risk-free-rate 0.02
  (buy 1 call #:strike 145 #:expiration 10 #:premium 1))

(define (3dtest2)
  (graph-decision
   (list (list call-alone "Long Call" "purple"))
   #:3d #f
   #:days-since-purchase 1000))

(define-option-strategy covered-call-test
  #:ticker 'AAPL
  #:ticker-price 150
  (buy 100 shares)
  (sell 1 call #:strike 160 #:expiration 1000))

(define-option-strategy protective-put-test
  #:ticker 'AAPL
  #:ticker-price 150
  (buy 100 shares)
  (buy 1 put #:strike 140 #:expiration 30))

(define-option-strategy synthetic-short-put
  #:ticker 'AAPL
  #:ticker-price 150
  (sell 100 shares)
  (buy 1 call #:strike 150 #:expiration 30))

(define (share-test)
  (graph-decision
   (list (list covered-call-test "Covered Call" "blue")
         (list protective-put-test "Protective Put" "green")
         (list synthetic-short-put "Synthetic Short Put" "red"))
   #:3d #f))


(provide define-option-strategy
         calculate-premium
         option-payoff)



;; Failed shortcut mode examples

#;
(define-option-strategy bad-call-debit
  #:ticker 'AAPL
  #:ticker-price 150
  #:quantity 1
  (call-debit-spread 160 150 30))

#;
(define-option-strategy bad-put-debit
  #:ticker 'AAPL
  #:ticker-price 150
  #:quantity 1
  (put-debit-spread 140 150 30)) ; Invalid: buy 140 < sell 150

#;
(define-option-strategy bad-call-credit
  #:ticker 'AAPL
  #:ticker-price 150
  #:quantity 1
  (call-credit-spread 140 150 30)) ; Invalid: sell 140 < buy 150

#;
(define-option-strategy bad-put-credit
  #:ticker 'AAPL
  #:ticker-price 150
  #:quantity 1
  (put-credit-spread 150 140 30)) ; Invalid: sell 150 > buy 140

#;
(define-option-strategy bad-butterfly
  #:ticker 'AAPL
  #:ticker-price 150
  #:quantity 1
  (butterfly-spread 150 150 160 30)) ; Invalid: k1 == k2

#;
(define-option-strategy bad-butterfly-order
  #:ticker 'AAPL
  #:ticker-price 150
  #:quantity 1
  (butterfly-spread 160 150 140 30)) ; Invalid: out of order

#;
(define-option-strategy bad-iron-condor
  #:ticker 'AAPL
  #:ticker-price 150
  #:quantity 1
  (iron-condor 130 120 140 150 30)) ; Invalid: 130 > 120
#;
(define-option-strategy bad-long-strangle
  #:ticker 'AAPL
  #:ticker-price 150
  #:quantity 1
  (long-strangle 160 140 30)) ; Invalid: put 160 > call 140


#;
(graph-preview-single)
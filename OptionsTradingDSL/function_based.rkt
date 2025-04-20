#lang racket

;; -----------------------------------------------------------------------------
;;  • Replaces compile‐time macros with plain Racket functions.
;;      – make-strategy/shortcut   → convenient one‐liner for canned spreads.
;;      – make-strategy            → fully‐specified legs using helpers (buy/sell).
;; -----------------------------------------------------------------------------

(require racket/match
         racket/date
         plot
         math/special-functions
         rackunit
         rackunit/text-ui)

;; -----------------------------------------------------------------------------
;; Symbol constants so users can write `call`, `put`, and `shares` without quotes
;; -----------------------------------------------------------------------------
(define call   'call)
(define put    'put)
(define shares 'shares)
(define buy-action  'buy)
(define sell-action 'sell)

;; -----------------------------------------------------------------------------
;; Core data structures
;; -----------------------------------------------------------------------------
(struct strategy    (name ticker ticker-price safe-mode volatility risk-free-rate legs)
  #:transparent)
(struct option-leg  (action qty type strike expiration premium)
  #:transparent)
(struct shares-leg  (action qty)
  #:transparent)

(define (positive-integer? v)     (and (integer? v)   (positive? v)))
(define (non‐neg-number? v)       (and (number? v)    (not (negative? v))))
(define (positive-number? v)      (and (number? v)    (positive? v)))
(define (buy/sell-symbol? v)      (member v '(buy sell)))
(define (option-type-symbol? v)   (member v '(call put)))
(define (boolean? v)              (or (eq? v #t) (eq? v #f))) ; Racket’s built‐in

;; Generic checker to keep the buy/sell helpers tidy
(define (assert pred? val who what)
  (unless (pred? val)
    (error who (format "~a: ~a" what val))))

;; -----------------------------------------------------------------------------
;; Leg constructors – provide ergonomic `buy` / `sell` functions
;; -----------------------------------------------------------------------------
(define (buy qty asset #:strike [strike #f] #:expiration [expiration #f]
                           #:premium  [premium  #f])
  (assert positive-integer? qty   'buy  "quantity must be a positive integer")
  (assert buy/sell-symbol?  'buy  'buy  "internal action check failed")

  (cond
    [(eq? asset shares)
     (shares-leg 'buy qty)]

    [(option-type-symbol? asset)
     (assert positive-number? strike     'buy "strike must be a positive number")
     (assert positive-integer? expiration 'buy "expiration (days) must be a positive integer")
     (when premium
       (assert non‐neg-number? premium   'buy "premium must be ≥ 0"))
     (option-leg 'buy qty asset strike expiration premium)]

    [else (error 'buy (format "unsupported asset kind: ~a" asset))]))

(define (sell qty asset #:strike [strike #f] #:expiration [expiration #f]
                            #:premium [premium #f])
  (assert positive-integer? qty   'sell "quantity must be a positive integer")

  (cond
    [(eq? asset shares)
     (shares-leg 'sell qty)]

    [(option-type-symbol? asset)
     (assert positive-number? strike     'sell "strike must be a positive number")
     (assert positive-integer? expiration 'sell "expiration (days) must be a positive integer")
     (when premium
       (assert non‐neg-number? premium   'sell "premium must be ≥ 0"))
     (option-leg 'sell qty asset strike expiration premium)]

    [else (error 'sell (format "unsupported asset kind: ~a" asset))]))

;; -----------------------------------------------------------------------------
;; Runtime validation helpers
;; -----------------------------------------------------------------------------
(define (strike<? a b) (and (number? a) (number? b) (< a b)))
(define (strike>? a b) (and (number? a) (number? b) (> a b)))

(define (validate-strategy-args strategy args)
  (match strategy
    ['call-debit-spread
     (match args
       [(list buy-strike sell-strike _)
        (unless (strike<? buy-strike sell-strike)
          (error 'make-strategy/shortcut "call‐debit‐spread: buy strike must be < sell strike"))])]
    ['put-debit-spread
     (match args
       [(list buy-strike sell-strike _)
        (unless (strike>? buy-strike sell-strike)
          (error 'make-strategy/shortcut "put‐debit‐spread: buy strike must be > sell strike"))])]
    ['call-credit-spread
     (match args
       [(list sell-strike buy-strike _)
        (unless (strike>? sell-strike buy-strike)
          (error 'make-strategy/shortcut "call‐credit‐spread: sell strike must be > buy strike"))])]
    ['put-credit-spread
     (match args
       [(list sell-strike buy-strike _)
        (unless (strike<? sell-strike buy-strike)
          (error 'make-strategy/shortcut "put‐credit‐spread: sell strike must be < buy strike"))])]
    ['butterfly-spread
     (match args
       [(list k1 k2 k3 _)
        (unless (and (strike<? k1 k2) (strike<? k2 k3) (not (= k1 k2)) (not (= k2 k3)))
          (error 'make-strategy/shortcut "butterfly: strikes must be distinct and ascending (k1 < k2 < k3)"))])]
    ['iron-condor
     (match args
       [(list k1 k2 k3 k4 _)
        (unless (and (strike<? k1 k2) (strike<? k2 k3) (strike<? k3 k4))
          (error 'make-strategy/shortcut "iron‐condor: strikes must ascend (k1 < k2 < k3 < k4)"))])]
    ['long-strangle
     (match args
       [(list put-strike call-strike _)
        (unless (strike<? put-strike call-strike)
          (error 'make-strategy/shortcut "long‐strangle: put strike must be < call strike"))])]
    [_ (void)]))

;; -----------------------------------------------------------------------------
;; Expand canned strategies into legs
;; -----------------------------------------------------------------------------
(define (expand-strategy-legs strategy args qty)
  (validate-strategy-args strategy args)
  (define qty*100 (* qty 100))
  (define qty*2   (* qty 2))
  (match strategy
    ['call-debit-spread
     (match args
       [(list k1 k2 exp)
        (list (buy  qty call #:strike k1 #:expiration exp)
              (sell qty call #:strike k2 #:expiration exp))])]
    ['put-debit-spread
     (match args
       [(list k1 k2 exp)
        (list (buy  qty put #:strike k1 #:expiration exp)
              (sell qty put #:strike k2 #:expiration exp))])]
    ['butterfly-spread
     (match args
       [(list k1 k2 k3 exp)
        (list (buy  qty   call #:strike k1 #:expiration exp)
              (sell qty*2 call #:strike k2 #:expiration exp)
              (buy  qty   call #:strike k3 #:expiration exp))])]
    ['call-credit-spread
     (match args
       [(list k1 k2 exp)
        (list (sell qty call #:strike k1 #:expiration exp)
              (buy  qty call #:strike k2 #:expiration exp))])]
    ['put-credit-spread
     (match args
       [(list k1 k2 exp)
        (list (sell qty put #:strike k1 #:expiration exp)
              (buy  qty put #:strike k2 #:expiration exp))])]
    ['iron-condor
     (match args
       [(list k1 k2 k3 k4 exp)
        (list (buy  qty put  #:strike k1 #:expiration exp)
              (sell qty put  #:strike k2 #:expiration exp)
              (sell qty call #:strike k3 #:expiration exp)
              (buy  qty call #:strike k4 #:expiration exp))])]
    ['iron-butterfly
     (match args
       [(list k1 k2 k3 exp)
        (list (buy  qty put  #:strike k1 #:expiration exp)
              (sell qty put  #:strike k2 #:expiration exp)
              (sell qty call #:strike k2 #:expiration exp)
              (buy  qty call #:strike k3 #:expiration exp))])]
    ['long-straddle
     (match args
       [(list k exp)
        (list (buy qty call #:strike k #:expiration exp)
              (buy qty put  #:strike k #:expiration exp))])]
    ['long-strangle
     (match args
       [(list p k exp)
        (list (buy qty put  #:strike p #:expiration exp)
              (buy qty call #:strike k #:expiration exp))])]
    ['covered-call
     (match args
       [(list k exp)
        (list (buy  qty*100 shares)
              (sell qty call #:strike k #:expiration exp))])]
    ['collar
     (match args
       [(list lp-sc sc-str exp)
        (list (buy  qty*100 shares)
              (buy  qty put  #:strike lp-sc #:expiration exp)
              (sell qty call #:strike sc-str #:expiration exp))])]
    ['diagonal-call-spread
     (match args
       [(list near-k near-exp far-k far-exp)
        (list (buy  qty call #:strike far-k  #:expiration far-exp)
              (sell qty call #:strike near-k #:expiration near-exp))])]
    [_ (error 'expand-strategy-legs (format "Unknown strategy: ~a" strategy))]))

;; -----------------------------------------------------------------------------
;; Public constructors
;; -----------------------------------------------------------------------------
(define (make-strategy/shortcut name
                                #:ticker ticker
                                #:ticker-price cp
                                #:quantity [qty 1]
                                #:volatility [vol 0.2]
                                #:risk-free-rate [rfr 0.05]
                                strategy-type . args)
  (assert symbol? ticker          'make-strategy "ticker must be a symbol")
  (assert positive-number? cp     'make-strategy "ticker-price must be > 0")
  (assert non‐neg-number? vol     'make-strategy "volatility must be ≥ 0")
  (assert non‐neg-number? rfr     'make-strategy "risk‐free rate must be ≥ 0")
  (assert positive-integer? qty 'make-strategy/shortcut "quantity must be a positive integer")

  (strategy name ticker cp #f vol rfr (expand-strategy-legs strategy-type args qty)))

(define (make-strategy name
                       #:ticker ticker
                       #:ticker-price cp
                       #:volatility [vol 0.2]
                       #:risk-free-rate [rfr 0.05]
                       . legs)

  (assert symbol? ticker          'make-strategy "ticker must be a symbol")
  (assert positive-number? cp     'make-strategy "ticker-price must be > 0")
  (assert non‐neg-number? vol     'make-strategy "volatility must be ≥ 0")
  (assert non‐neg-number? rfr     'make-strategy "risk‐free rate must be ≥ 0")

  (strategy name ticker cp #f vol rfr legs))

;; -----------------------------------------------------------------------------
;; Pricing & payoff math (unchanged from original)
;; -----------------------------------------------------------------------------
(define (cdf x)
  (/ (+ 1 (erf (/ x (sqrt 2)))) 2))

(define (black-scholes-call S K T r sigma)
  (cond
    [(= T 0) (max 0 (- S K))]
    [(= sigma 0)
     (let ([discounted-K (* K (exp (* (- r) T)))] )
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
     (let ([discounted-K (* K (exp (* (- r) T)))] )
       (max 0 (- discounted-K S)))]
    [else
     (let* ([d1 (/ (+ (log (/ S K)) (* (+ r (/ (expt sigma 2) 2)) T))
                   (* sigma (sqrt T)))]
            [d2 (- d1 (* sigma (sqrt T)))]
            [Nd1 (cdf (- d1))]
            [Nd2 (cdf (- d2))]
            [discount-factor (exp (* (- r) T))])
       (- (* K discount-factor Nd2) (* S Nd1)))]))

(define (calculate-premium strike price expiration rfr vol type)
  (if (eq? type 'call)
      (black-scholes-call price strike expiration rfr vol)
      (black-scholes-put  price strike expiration rfr vol)))

(define (share-payoff stock-price action qty entry-price)
  (let ([delta (- stock-price entry-price)])
    (* qty (if (eq? action 'buy) delta (- delta)))) )

(define (option-value-at-time stock-price strike action type qty premium
                              rfr vol days-since orig-exp-days entry-price)
  (let* ([T (/ (- orig-exp-days days-since) 365.0)]
         [initial-T (/ orig-exp-days 365.0)]
         [cost-basis-per-contract (if premium
                                      premium
                                      (calculate-premium strike entry-price initial-T rfr vol type))]
         [bs-value (calculate-premium strike stock-price T rfr vol type)]
         [net-value (* qty 100 bs-value)]
         [cost      (* qty 100 cost-basis-per-contract)])
    (if (eq? action 'buy) (- net-value cost) (- cost net-value))))

(define (total-strategy-value-at-time strat stock-price days-remaining)
  (define legs           (strategy-legs strat))
  (define entry          (strategy-ticker-price strat))
  (define vol            (strategy-volatility   strat))
  (define rfr            (strategy-risk-free-rate strat))
  (for/sum ([leg legs])
    (cond
      [(option-leg? leg)
       (option-value-at-time stock-price
                             (option-leg-strike leg)
                             (option-leg-action leg)
                             (option-leg-type leg)
                             (option-leg-qty leg)
                             (option-leg-premium leg)
                             rfr vol days-remaining
                             (option-leg-expiration leg)
                             entry)]
      [(shares-leg? leg)
       (share-payoff stock-price (shares-leg-action leg) (shares-leg-qty leg) entry)]
      [else (error "Unknown leg type" leg)])))

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



;; -----------------------------------------------------------------------------
;; Example usage
;; -----------------------------------------------------------------------------
(define bullish-strat-shortened
  (make-strategy/shortcut 'bullish-strat-shortened
                          #:ticker 'AAPL
                          #:ticker-price 145.75
                          #:quantity 2
                          `call-debit-spread 140 150 7))

(define collar-shortened
  (make-strategy/shortcut 'collar-shortened
                          #:ticker 'AAPL
                          #:ticker-price 145.75
                          `collar 140 150 7))

(define decaying-call-spread
  (make-strategy 'decaying-call-spread
                 #:ticker 'AAPL
                 #:ticker-price 150
                 #:volatility 0.3
                 #:risk-free-rate 0.02
                 (buy 1 call #:strike 145 #:expiration 1000)
                 (sell 1 call #:strike 155 #:expiration 1000)))

(define (graph-preview-single)
  (graph-decision
   (list (list bullish-strat-shortened "Bull Call Spread" "blue"))
   #:3d #t))

(define covered-call-test
  (make-strategy 'covered-call-test
                 #:ticker 'AAPL
                 #:ticker-price 150
                 (buy 100 shares)
                 (sell 1 call #:strike 160 #:expiration 30)))

(define protective-put-test
  (make-strategy 'protective-put-test
                 #:ticker 'AAPL
                 #:ticker-price 150
                 (buy 100 shares)
                 (buy 1 put #:strike 140 #:expiration 30)))

(define synthetic-short-put
  (make-strategy 'synthetic-short-put
                 #:ticker 'AAPL
                 #:ticker-price 150
                 (sell 100 shares)
                 (buy 1 call #:strike 150 #:expiration 30)))

(define (share-test)
  (graph-decision
   (list (list covered-call-test      "Covered Call"          "blue")
         (list protective-put-test    "Protective Put"        "green")
         (list synthetic-short-put    "Synthetic Short Put"   "red"))
   #:3d #t))


#;
(define bad-call-debit
  (make-strategy/shortcut 'bad-call-debit
                          #:ticker 'AAPL
                          #:ticker-price 150
                          #:quantity 1
                          'call-debit-spread 160 150 30))

(provide
  (struct-out strategy)
  (struct-out option-leg)
  (struct-out shares-leg)
  make-strategy
  make-strategy/shortcut
  buy sell call put shares
  calculate-premium
  option-value-at-time
  total-strategy-value-at-time)


;; -----------------------------------------------------------------------------
;; Tests
;; -----------------------------------------------------------------------------

;; helper that produces a test‐case in one line
(define (expect-fail desc thunk)
  (test-case desc
    (check-exn exn:fail? thunk)))

(define-syntax-rule (check-≈ expr expected tol)
  (check-= expr expected tol))

(define validation-tests
  (test-suite
   "strategy runtime validation"

   ;; 0. positive build
   (test-case "valid strategy builds"
     (check-true
      (strategy? (make-strategy/shortcut 'ok
                                         #:ticker 'AAPL
                                         #:ticker-price 150
                                         'call-debit-spread 140 150 30))))

   ;; 1. header checks
   (expect-fail "ticker must be symbol"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker "AAPL"
                                   #:ticker-price 150
                                   'call-debit-spread 140 150 30)))

   (expect-fail "ticker-price > 0"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price -1
                                   'call-debit-spread 140 150 30)))

   (expect-fail "quantity > 0"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   #:quantity 0
                                   'call-debit-spread 140 150 30)))

   ;; 2. shortcut ordering / distinctness
   (expect-fail "call‐debit: buy < sell"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'call-debit-spread 160 150 30)))

   (expect-fail "put‐debit: buy > sell"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'put-debit-spread 140 150 30)))

   (expect-fail "call‐credit: sell ≤ buy"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'call-credit-spread 140 150 30)))

   (expect-fail "put‐credit: sell ≥ buy"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'put-credit-spread 150 140 30)))

   (expect-fail "butterfly: duplicate strikes"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'butterfly-spread 150 150 160 30)))

   (expect-fail "butterfly: out‐of‐order strikes"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'butterfly-spread 160 150 140 30)))

   (expect-fail "iron‐condor: strikes must ascend"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'iron-condor 130 120 140 150 30)))

   (expect-fail "long‐strangle: put < call"
     (λ () (make-strategy/shortcut 'bad
                                   #:ticker 'AAPL
                                   #:ticker-price 150
                                   'long-strangle 160 140 30)))

   ;; 3. leg constructor validation
   (expect-fail "negative quantity in buy"
     (λ () (make-strategy 'bad-leg
                          #:ticker 'AAPL
                          #:ticker-price 150
                          (buy -1 call #:strike 100 #:expiration 30))))

   (expect-fail "non‐numeric strike"
     (λ () (make-strategy 'bad-leg
                          #:ticker 'AAPL
                          #:ticker-price 150
                          (buy 1 call #:strike 'foo #:expiration 30))))
))


(define (≈ a b tol) (< (abs (- a b)) tol))
(define tol 0.001)

;; ------------------------------------------------------------
;; Premium tests
;; ------------------------------------------------------------
(define premium-tests
  (test-suite
   "Black–Scholes premium values (tolerance 1e‐3)"
   (check-true (≈ (calculate-premium 140 145.75 0.0822 0.02 0.3 'call) 8.4563 tol))
   (check-true (≈ (calculate-premium 150 145.75 0.0822 0.02 0.3 'call) 3.3159 tol))
   (check-true (≈ (calculate-premium 140 145.75 0.0822 0.02 0.3 'put)  2.4763 tol))
   (check-true (≈ (calculate-premium 150 145.75 0.0822 0.02 0.3 'put)  7.3195 tol))
   (check-true (≈ (calculate-premium 140 145.75 1       0.05 0.2 'call) 18.5054 tol))
   (check-true (≈ (calculate-premium 150 145.75 1       0.05 0.2 'put)  10.0195 tol))))

;; ------------------------------------------------------------
;; Option‐value tests
;; ------------------------------------------------------------
(define option-value-tests
  (test-suite
   "Black–Scholes value vs. payoff at expiry"
   (check-= (option-value-at-time 110 100 'buy  'call 1 5 0 0 365 365 100)  500 0.001)
   (check-= (option-value-at-time 110 100 'sell 'call 1 5 0 0 365 365 100) -500 0.001)
   (check-= (option-value-at-time  90 100 'buy  'put  1 3 0 0 365 365 100)  700 0.001)
   (check-= (option-value-at-time  90 100 'sell 'put  1 3 0 0 365 365 100) -700 0.001)
   (check-= (option-value-at-time  90 100 'buy  'call 1 5 0 0 365 365 100) -500 0.001)
   (check-= (option-value-at-time 100 100 'buy  'call 1 2 0 0 365 365 100) -200 0.001)
   (check-= (option-value-at-time 100 100 'sell 'call 1 2 0 0 365 365 100)  200 0.001)
   (check-= (option-value-at-time 110 100 'buy  'call 2 5 0 0 365 365 100) 1000 0.001)
   ;; time value example
   (check-= (option-value-at-time 105 100 'buy 'call 1 #f 0.05 0.3 0 365 100)
            327.38 0.5)))

;; ------------------------------------------------------------
;; Total‐strategy tests
;; ------------------------------------------------------------
(define total-strategy-value-tests
  (test-suite
   "total-strategy-value-at-time consistency"
   (check-≈
    (total-strategy-value-at-time
     (strategy 'test 'AAPL 100 #f 0.3 0.05
               (list (option-leg 'buy 1 'call 90 30 #f)))
     105 0)
    (option-value-at-time 105 90 'buy 'call 1 #f 0.05 0.3 0 30 100)
    0.01)

   (check-≈
    (total-strategy-value-at-time
     (strategy 'test2 'AAPL 100 #f 0.3 0.05
               (list (option-leg 'buy 1 'put 110 30 #f)))
     100 0)
    (option-value-at-time 100 110 'buy 'put 1 #f 0.05 0.3 0 30 100)
    0.01)

   (check-≈
    (total-strategy-value-at-time
     (strategy 'covered 'AAPL 100 #f 0.2 0.05
               (list (shares-leg 'buy 100)
                     (option-leg 'sell 1 'call 110 10 #f)))
     115 0)
    (+ (share-payoff 115 'buy 100 100)
       (option-value-at-time 115 110 'sell 'call 1 #f 0.05 0.2 0 10 100))
    0.01)

   (check-≈
    (total-strategy-value-at-time
     (strategy 'short-put 'AAPL 100 #f 0.25 0.05
               (list (option-leg 'sell 1 'put 90 30 #f)))
     100 0)
    (option-value-at-time 100 90 'sell 'put 1 #f 0.05 0.25 0 30 100)
    0.01)

   (check-≈
    (total-strategy-value-at-time
     (strategy 'long-call-time 'AAPL 100 #f 0.4 0.05
               (list (option-leg 'buy 1 'call 120 365 #f)))
     105 0)
    (option-value-at-time 105 120 'buy 'call 1 #f 0.05 0.4 0 365 100)
    0.01)))

(define all-tests
  (test-suite
   "All option‐strategy tests"
   validation-tests
   premium-tests
   option-value-tests
   total-strategy-value-tests))


(module+ test
  (run-tests all-tests 'normal))





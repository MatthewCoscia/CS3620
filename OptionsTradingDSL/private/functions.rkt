#lang racket
(require math/special-functions)
;; -----------------------------------------------------------------------------
;; Pricing & payoff math
;; -----------------------------------------------------------------------------

(struct strategy    (name ticker ticker-price safe-mode volatility risk-free-rate legs)
  #:transparent)
(struct option-leg  (action qty type strike expiration premium)
  #:transparent)
(struct shares-leg  (action qty)
  #:transparent)

(define call   'call)
(define put    'put)
(define shares 'shares)
(define buy-action  'buy)
(define sell-action 'sell)
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

;; Generic checker to keep the buy/sell helpers tidy
(define (assert pred? val who what)
  (unless (pred? val)
    (error who (format "~a: ~a" what val))))



(define (positive-integer? v)     (and (integer? v)   (positive? v)))
(define (non‐neg-number? v)       (and (number? v)    (not (negative? v))))
(define (positive-number? v)      (and (number? v)    (positive? v)))
(define (buy/sell-symbol? v)      (member v '(buy sell)))
(define (option-type-symbol? v)   (member v '(call put)))

(provide 
  (struct-out strategy)
  (struct-out option-leg)
  (struct-out shares-leg)
  calculate-premium
  option-value-at-time
  share-payoff
  total-strategy-value-at-time
  buy
  sell
  call
  put
  assert
  shares
  get-plot-bounds
  find-breakeven-function
  expand-strategy-legs
  positive-integer?
  non‐neg-number?
  positive-number?
  buy/sell-symbol?
  option-type-symbol?)


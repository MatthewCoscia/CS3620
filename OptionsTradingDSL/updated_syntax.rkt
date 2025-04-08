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
    (pattern (action:action
              qty:positive-whole-qty
              type:option-type
              #:strike s:expr
              #:expiration exp:expr
              (~optional (~seq #:premium p:non-negative-premium)
                         #:defaults ([p #'#f])))
             #:with result #'(option-leg 'action.act qty 'type.t s exp p))
    (pattern (action:action
              qty:positive-whole-qty
              #:shares)
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
                   #:defaults ([rfr #'0.05]))   ;; Default Risk-Free Rate: 5%
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






(define (total-strategy-payoff strat stock-price)
  (let* ([legs (strategy-legs strat)]
         [ticker-price (strategy-ticker-price strat)]
         [volatility (strategy-volatility strat)]
         [risk-free-rate (strategy-risk-free-rate strat)])
    (apply +
           (map (λ (leg)
                  (option-payoff stock-price
                                 (option-leg-strike leg)
                                 (option-leg-action leg)
                                 (option-leg-type leg)
                                 (option-leg-qty leg)
                                 (option-leg-premium leg)
                                 risk-free-rate
                                 volatility
                                 (option-leg-expiration leg)
                                 ticker-price))
                legs))))




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

     ;; Convert syntax to raw Racket values
     (define strategy (syntax-e #'strategy-type))
     (define args-list (map syntax-e (syntax->list #'(args ...))))

     ;; Here’s the crucial difference:
     ;; We generate the actual legs by calling the run-time function.
     (define legs (expand-strategy-legs strategy args-list))

     #`(define-option-strategy strategy-name
         #:ticker         ticker
         #:ticker-price   cp
         #:safe-mode      safe
         #:volatility     vol
         #:risk-free-rate rfr
         #,@legs)]))
    ;; Already-defined strategies:



;; Helper 1: Calculate plot boundaries
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


;; Helper 2: Create plot elements for a single strategy
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


;; Example that would error
(define-option-strategy risky-strat
  #:ticker 'TSLA
  #:ticker-price 250.50
  #:safe-mode #t
  (buy 1 call #:strike 300 #:expiration 30)  ;; Naked call
  (sell 1 call  #:strike 200 #:expiration 30)) ;; Naked put

;; Valid covered strategy
(define-option-strategy safe-strat
  #:ticker 'GOOG
  #:ticker-price 145.75
  #:safe-mode #t
  (sell 1 call #:strike 150 #:expiration 30)
  (buy  1 call #:strike 150 #:expiration 30)  ;; Covers call
  (sell 1 put  #:strike 140 #:expiration 30)
  (buy  1 put  #:strike 140 #:expiration 30)) ;; Covers put

(define-option-strategy bullish-strat
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  (buy 1 call #:strike 140 #:expiration 30)  ;; Lower strike, long call
  (sell 1 call #:strike 150 #:expiration 30)) ;; Higher strike, short call




(define-option-strategy high-vol-strat
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  #:volatility 0.3  ;; 30% volatility
  #:risk-free-rate 0.02  ;; 2% risk-free rate
  (buy 1 call #:strike 140 #:expiration 30)
  (sell 1 call #:strike 150 #:expiration 30))

(define-option-strategy high-vol-strat-prem
  #:ticker 'AAPL
  #:ticker-price 280.75
  #:safe-mode #t
  #:volatility 0.3  ;; 30% volatility
  #:risk-free-rate 0.02  ;; 2% risk-free rate
  (buy 1 call #:strike 240 #:expiration 30 #:premium 7.50)
  (sell 1 call #:strike 320 #:expiration 30 #:premium 5.00))

(define-option-strategy-shortcuts bullish-strat-shortened
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  (call-debit-spread 140 150 7))

(define-option-strategy-shortcuts collar-shortened
  #:ticker 'AAPL
  #:ticker-price 145.75
  #:safe-mode #t
  (call-debit-spread 140 150 7))

(define (graph-preview)
  (graph-multiple-strategies
   (list (list high-vol-strat-prem "Bull Call Spread" "blue"))
   #:min-price 50  ; Optional manual price range
   #:max-price 350))




(define (graph-preview-single)
  (graph-multiple-strategies
   (list (list bullish-strat "Bull Call Spread" "blue"))))

(define (≈ a b tol) (< (abs (- a b)) tol))

(define tol 0.001)

(define-test-suite premium-tests
  (check-true (≈ (calculate-premium 140 145.75 0.0822 0.02 0.3 'call)
                 8.4563 tol))
  (check-true (≈ (calculate-premium 150 145.75 0.0822 0.02 0.3 'call)
                 3.3159 tol))
  (check-true (≈ (calculate-premium 140 145.75 0.0822 0.02 0.3 'put)
                 2.4763 tol))
  (check-true (≈ (calculate-premium 150 145.75 0.0822 0.02 0.3 'put)
                 7.3195 tol))
  (check-true (≈ (calculate-premium 140 145.75 1 0.05 0.2 'call)
                 18.5054 tol))
  (check-true (≈ (calculate-premium 150 145.75 1 0.05 0.2 'put)
                 10.0195 tol)))

(define-test-suite payoff-tests
  (check-= (option-payoff 110 100 'buy 'call 1 5 0 0 365 100) 5 0.001)
  (check-= (option-payoff 110 100 'sell 'call 1 5 0 0 365 100) -5 0.001)
  (check-= (option-payoff 90 100 'buy 'put 1 3 0 0 365 100) 7 0.001)
  (check-= (option-payoff 90 100 'sell 'put 1 3 0 0 365 100) -7 0.001)
  (check-= (option-payoff 90 100 'buy 'call 1 5 0 0 365 100) -5 0.001)
  (check-= (option-payoff 100 100 'buy 'call 1 2 0 0 365 100) -2 0.001)
  (check-= (option-payoff 100 100 'sell 'call 1 2 0 0 365 100) 2 0.001)
  (check-= (option-payoff 110 100 'buy 'call 2 5 0 0 365 100) 10 0.001)
  (check-= (option-payoff 105 100 'buy 'call 1 #f 0.05 0 365 100) 0.123 0.01))


;; Example Fails

(define (fails-to-compile? expr)
  (with-handlers ([exn:fail? (λ (_) #t)]) ;; If an exception occurs, return #t
    (eval expr)  ;; Try to evaluate the expression
    #f))         ;; If it compiles, return #f (which is a failure in our test)

(define-test-suite safe-mode-failure-tests

  ;; Cannot sell more contracts than total purchased (no over-leveraging)
  (check-true (fails-to-compile?
               '(define-option-strategy over-leveraged
                  #:ticker 'TSLA
                  #:ticker-price 700
                  #:safe-mode #t
                  (buy 2 call #:strike 750 #:expiration 30)
                  (sell 5 call #:strike 750 #:expiration 30)))
              ;; Selling more than buying
              "Safe mode: Should fail because more options
\are sold than bought.")

  ;; Strike price must be within 30% of current price
  (check-true (fails-to-compile?
               '(define-option-strategy too-far-otm
                  #:ticker 'NFLX
                  #:ticker-price 500
                  #:safe-mode #t
                  (buy 1 call #:strike 700 #:expiration 60)))
              ;; 40% away from current price
              "Safe mode: Should fail because strike price is too high.")

  (check-true (fails-to-compile?
               '(define-option-strategy too-far-it
                  #:ticker 'NFLX
                  #:ticker-price 500
                  #:safe-mode #t
                  (buy 1 put #:strike 250 #:expiration 60)))
              ;; Strike too far from current price
              "Safe mode: Should fail because strike price is too low.")

  ;; Naked short calls or puts are not allowed
  (check-true (fails-to-compile?
               '(define-option-strategy naked-call
                  #:ticker 'AMZN
                  #:ticker-price 3000
                  #:safe-mode #t
                  (sell 1 call #:strike 3100 #:expiration 30)))
              ;; No corresponding "buy"
              "Safe mode: Should fail because selling a naked call.")

  (check-true (fails-to-compile?
               '(define-option-strategy naked-put
                  #:ticker 'GOOG
                  #:ticker-price 2800
                  #:safe-mode #t
                  (sell 1 put #:strike 2700 #:expiration 30)))
              ;; No corresponding "buy"
              "Safe mode: Should fail because selling a naked put.")
  )

;; Run all tests with verbose output
(displayln "Running Premium Tests:")
(run-tests premium-tests 'verbose)
(newline)
(displayln "Running Payoff Tests:")
(run-tests payoff-tests 'verbose)
(newline)
(displayln "Compilation Error Tests:")
(run-tests safe-mode-failure-tests 'verbose)

(graph-preview)
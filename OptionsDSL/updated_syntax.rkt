#lang racket

(require (for-syntax syntax/parse)
         racket/date
         plot
         rackunit
         rackunit/text-ui
         math/special-functions)

;; ----------------------------------------
;; 1. Overview: Compilation Process
;; ----------------------------------------
;; This DSL compiles options trading strategies into structured Racket hash tables.
;; The compilation process involves:
;; - **Parsing the DSL Syntax** using `syntax-parse` and syntax classes.
;; - **Performing static validation checks** (e.g., preventing naked shorts, over-leveraging).
;; - **Transforming strategies into Racket data structures** for runtime evaluation.
;; - **Providing runtime functions** to compute payoffs and generate graphs.

;; ----------------------------------------
;; 2. Parsing: Syntax Structure
;; ----------------------------------------
;; The DSL is defined using a macro `define-option-strategy`, which takes:
;; - A ticker symbol (`#:ticker`)
;; - A stock price (`#:current-price`)
;; - A safety flag (`#:safe-mode`)
;; - Optional parameters like `#:volatility` and `#:risk-free-rate`
;; - A sequence of trades (e.g., `(buy 1 call #:strike 150 #:expiration 30)`)

;; ----------------------------------------
;; 2.1 Single Option Trade Definition
;; ----------------------------------------
;; Each option trade follows this syntax:
;;
;; (ACTION QTY TYPE #:strike STRIKE #:expiration EXPIRATION [#:premium PREMIUM])
;;
;; Where:
;; - `ACTION`: Either `buy` or `sell`, defining if the contract is purchased or written.
;; - `QTY`: A positive integer specifying the number of contracts.
;; - `TYPE`: `call` or `put`, determining the option type.
;; - `#:strike STRIKE`: The strike price at which the option can be exercised.
;; - `#:expiration EXPIRATION`: The expiration time in days.
;; - `#:premium PREMIUM` (optional): The premium paid/received per contract.

;; Syntax validation is handled using **syntax classes**:
;; - `action`: Restricts valid trade actions to `buy` or `sell`.
;; - `option-type`: Ensures options are either `call` or `put`.
;; - `ticker-symbol`: Enforces correct ticker syntax (quoted symbol).
;; - `positive-price`, `positive-whole-qty`: Ensure valid price and contract quantity.

;; ----------------------------------------
;; 3. Compilation: Expanding to Racket Hash Tables
;; ----------------------------------------
;; The macro expands a strategy into a structured `hash` storing:
;; - `ticker`: The underlying asset.
;; - `current-price`: The market price of the asset.
;; - `safe-mode`: Whether risk constraints apply.
;; - `volatility`, `risk-free-rate`: Option pricing parameters.
;; - `legs`: A list of trades (buy/sell option contracts).

;; ----------------------------------------
;; 4. Static Validation Checks
;; ----------------------------------------
;; Several **compile-time restrictions** (`#:fail-when`) ensure valid strategies:
;; - **Expiration Range Check**: Options must expire between 7-365 days.
;; - **No Over-Leveraging**: Cannot sell more contracts than purchased.
;; - **Strike Price Limits**: Strike price must be within 30% of stock price.
;; - **No Naked Shorts**: Selling options requires owning a corresponding long position.

;; ----------------------------------------
;; 5. Runtime Functions: Processing & Visualization
;; ----------------------------------------
;; Once compiled, the strategy can be:
;; - Evaluated using `total-strategy-payoff` to compute expected gains/losses.
;; - Visualized using `graph-multiple-strategies` to display option payoff structures.
;; - Parsed using `parse-options` to extract structured trade data.

;; This structure ensures **concise, readable DSL syntax** while providing **strong safety checks**
;; and **efficient runtime execution**.


;;Grammar:
#;
(define-option-strategy strategy-name
  #:ticker 'SYMBOL
  #:current-price PRICE
  #:safe-mode BOOLEAN
  [#:volatility VOLATILITY]
  [#:risk-free-rate RFR]
  TRADE ...)

#;
(ACTION QTY TYPE #:strike STRIKE #:expiration EXPIRATION [#:premium PREMIUM])

#;
(graph-multiple-strategies
  (list STRATEGY-PLOT ...)
  [#:min-price MIN]
  [#:max-price MAX])


(begin-for-syntax
  (define-syntax-class action
    (pattern (~or buy sell)))
  
  (define-syntax-class option-type
    (pattern (~or call put)))

  (define-syntax-class non-negative-premium
  #:description "non-negative premium"
  (pattern p:expr
           #:fail-when (and (number? (syntax-e #'p))
                            (< (syntax-e #'p) 0))
           "Premium must be non-negative."))

  (define-syntax-class positive-whole-qty
    #:description "positive whole number"
    (pattern q:expr
             #:fail-when (let ([v (syntax-e #'q)])
                           (not (exact-positive-integer? v)))
             "must be positive integer"))
  
  (define-syntax-class ticker-symbol
    #:description "ticker symbol"
    (pattern t:expr
             #:when (let ([v (syntax->datum #'t)])
                      (and (pair? v) 
                           (eq? (car v) 'quote)
                           (symbol? (cadr v))))
             #:with ticker-value (let ([v (syntax->datum #'t)])
                                   (cadr v))))
  
  (define-syntax-class positive-price
    #:description "positive price"
    (pattern p:expr
             #:fail-when (let ([v (syntax-e #'p)])
                           (and (number? v)
                                (not (positive? v))))
             "price must be positive"))
             
  (define-syntax-class safe-mode
    #:description "safety mode setting"
    (pattern (~or #t #f))))

(define-syntax (define-option-strategy stx)
  (syntax-parse stx
    [(_ strategy-name:id 
        #:ticker ticker:ticker-symbol
        #:current-price cp:positive-price
        #:safe-mode safe:safe-mode
        (~optional [~seq #:volatility vol:expr] #:defaults ([vol #'0.2])) ;; Default Volatility: 20%
        (~optional [~seq #:risk-free-rate rfr:expr] #:defaults ([rfr #'0.05])) ;; Default Risk-Free Rate: 5%
        (action:action qty:positive-whole-qty 
                       type:option-type 
                       #:strike s:expr 
                       #:expiration exp:number
                       (~optional [~seq #:premium p:non-negative-premium] #:defaults ([p #'#f]))) ...)
     
     #:with (action-sym ...) (map (λ (a) (datum->syntax #f (syntax->datum a))) 
                                  (syntax->list #'(action ...)))
     #:with (type-sym ...) (map (λ (t) (datum->syntax #f (syntax->datum t))) 
                                (syntax->list #'(type ...)))
     #:with (premium-sym ...)
     (map (λ (p-stx)
            (if (equal? (syntax->datum p-stx) #f)
                #'#f  ;; Flag for later calculation
                p-stx))
          (syntax->list #'(p ...)))
     
     ;; Combined check for both naked calls and puts
     #:fail-when (and (equal? (syntax-e #'safe) #t)
                      (for/or ([exp (syntax->list #'(exp ...))])
                        (or (< (syntax-e exp) 7)
                            (> (syntax-e exp) 365))))
     "Expiration date must be between 7 and 365 days"


     #:fail-when (and (equal? (syntax-e #'safe) #t)
                      (for/or ([a (syntax->list #'(action ...))]
                               [q (syntax->list #'(qty ...))])
                        (and (eq? (syntax-e a) 'sell)
                             (> (syntax-e q) (apply + (map syntax-e (syntax->list #'(qty ...))))))))
     "Safe mode: Cannot sell more option contracts than total purchased (no over-leveraging)."


     #:fail-when (and (equal? (syntax-e #'safe) #t)
                      (let ([cp (syntax-e #'cp)])  ;; Extract current price once
                        (for/or ([s (syntax->list #'(s ...))])
                          (> (abs (- (syntax-e s) cp)) (* cp 0.3))))) ;; Compare strikes to price
     "Safe mode: Strike price must be within 30% of the current price."


     #:fail-when (and (equal? (syntax-e #'safe) #t)
                      (or
                       (for/or ([a (syntax->list #'(action ...))]
                                [t (syntax->list #'(type ...))])
                         (and (eq? (syntax-e a) 'sell)
                              (eq? (syntax-e t) 'put)
                              (not (for/or ([a2 (syntax->list #'(action ...))]
                                            [t2 (syntax->list #'(type ...))]
                                            [q2 (syntax->list #'(qty ...))])
                                     (and (eq? (syntax-e a2) 'buy)
                                          (eq? (syntax-e t2) 'put)
                                          (>= (syntax-e q2) 1))))))

                       (for/or ([a (syntax->list #'(action ...))]
                                [t (syntax->list #'(type ...))])
                         (and (eq? (syntax-e a) 'sell)
                              (eq? (syntax-e t) 'call)
                              (not (for/or ([a2 (syntax->list #'(action ...))]
                                            [t2 (syntax->list #'(type ...))]
                                            [q2 (syntax->list #'(qty ...))])
                                     (and (eq? (syntax-e a2) 'buy)
                                          (eq? (syntax-e t2) 'call)
                                          (>= (syntax-e q2) 1))))))))
     "Naked short calls or puts are not allowed in safe mode"
     
     #'(define strategy-name
         (hash 'ticker 'ticker-value
               'current-price cp
               'safe-mode safe
               'volatility vol
               'risk-free-rate rfr
               'legs (list (list 'action-sym qty 'type-sym s exp p) ...)))]))

(define (parse-options strategy)
  (map (lambda (leg)
         (match leg
           [(list action qty type strike exp-days premium)
            (hash 'action action
                  'quantity qty
                  'type type
                  'strike strike
                  'expiration exp-days
                  'premium premium)]))
       (hash-ref strategy 'legs)))




(define (option-payoff stock-price strike action type quantity premium 
                       risk-free-rate volatility expiration-days current-price)
  (let* ([T (/ expiration-days 365.0)]  ;; Convert days to years
         [actual-premium (if (equal? premium #f)
                             (calculate-premium strike current-price T 
                                                risk-free-rate volatility type)
                             premium)]  ;; Use either Black-Scholes or manual premium
         [raw-payoff (cond
                       [(eq? type 'call) (max 0 (- stock-price strike))]
                       [(eq? type 'put)  (max 0 (- strike stock-price))])]
         [intrinsic-value (* quantity raw-payoff)]  ;; Pure option value
         [initial-cost (* quantity actual-premium)])  ;; Cost upfront
    
    ;; Adjust for long and short call behavior
    (if (eq? action 'buy)
        (- intrinsic-value initial-cost)  ;; Buying: Pay premium upfront
        (- initial-cost intrinsic-value))))  ;; Selling: Limit gains beyond strike






(define (total-strategy-payoff strategy stock-price)
  (let* ([legs (parse-options strategy)]
         [current-price (hash-ref strategy 'current-price)]
         [volatility (hash-ref strategy 'volatility)]
         [risk-free-rate (hash-ref strategy 'risk-free-rate)])
    (apply +
           (map (lambda (leg)
                  (option-payoff stock-price
                                 (hash-ref leg 'strike)
                                 (hash-ref leg 'action)
                                 (hash-ref leg 'type)
                                 (hash-ref leg 'quantity)
                                 (hash-ref leg 'premium)
                                 risk-free-rate
                                 volatility
                                 (hash-ref leg 'expiration)
                                 current-price))
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

(define (graph-strategy strategy)
  (define min-price (- (hash-ref strategy 'current-price) 50))
  (define max-price (+ (hash-ref strategy 'current-price) 50))
  (define step 1)

  ;; Find breakeven points
  (define breakevens (find-breakeven strategy min-price max-price step))

  ;; Define payoff function
  (define (payoff x) (total-strategy-payoff strategy x))

  ;; Plot main payoff function + breakeven points
  (plot (list
         (function payoff min-price max-price)
         (points (map (lambda (x) (vector x (payoff x))) breakevens)
                 #:sym 'circle
                 #:size 8
                 #:color "red"))
        #:title "Option Strategy Payoff"
        #:x-label "Stock Price"
        #:y-label "Profit/Loss"))

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



(define (calculate-premium strike price expiration risk-free-rate volatility type)
  (let ([premium (if (eq? type 'call)
                     (black-scholes-call price strike expiration risk-free-rate volatility)
                     (black-scholes-put price strike expiration risk-free-rate volatility))])
    premium))


(define (print-strategy strategy)
  (define min-price (- (hash-ref strategy 'current-price) 50))
  (define max-price (+ (hash-ref strategy 'current-price) 50))
  (define step 1)

  ;; Generate (stock price, payoff) pairs
  (for ([price (in-range min-price (+ max-price step) step)])
    (printf "Stock Price: ~a | Payoff: ~a\n"
            price
            (total-strategy-payoff strategy price))))


;; Helper 1: Calculate plot boundaries
(define (get-plot-bounds strategies min-price max-price)
  (if (or min-price max-price)
      (values (or min-price (- (hash-ref (caar strategies) 'current-price) 50))
              (or max-price (+ (hash-ref (caar strategies) 'current-price) 50)))
      (let ([current-prices (map (λ (s) (hash-ref (car s) 'current-price)) strategies)])
        (values (- (apply min current-prices) 50)
                (+ (apply max current-prices) 50)))))

;; Helper 2: Create plot elements for a single strategy
(define (make-single-strategy-plot strategy label color x-min x-max)
  (define (payoff x) (total-strategy-payoff strategy x))
  (define breakevens (find-breakeven strategy x-min x-max 1))
  (list (function payoff #:label label #:color color)
        (points (map (λ (x) (vector x (payoff x))) breakevens)
                #:sym 'circle #:size 8 #:color color #:label #f)))

;; Main function
(define (graph-multiple-strategies strategies #:min-price [min-price #f] #:max-price [max-price #f])
  (let-values ([(x-min x-max) (get-plot-bounds strategies min-price max-price)])
    (parameterize ([plot-new-window? #t])  ;; Ensure graph opens in a new window
      (plot (append-map (λ (strat-info)
                          (match-define (list strategy label color) strat-info)
                          (make-single-strategy-plot strategy label color x-min x-max))
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
  #:current-price 250.50
  #:safe-mode #t
  (buy 1 call #:strike 300 #:expiration 30)  ;; Naked call
  (sell 1 call  #:strike 200 #:expiration 30)) ;; Naked put

;; Valid covered strategy
(define-option-strategy safe-strat
  #:ticker 'GOOG
  #:current-price 145.75
  #:safe-mode #t
  (sell 1 call #:strike 150 #:expiration 30)
  (buy  1 call #:strike 150 #:expiration 30)  ;; Covers call
  (sell 1 put  #:strike 140 #:expiration 30)
  (buy  1 put  #:strike 140 #:expiration 30)) ;; Covers put

(define-option-strategy bullish-strat
  #:ticker 'AAPL
  #:current-price 145.75
  #:safe-mode #t
  (buy 1 call #:strike 140 #:expiration 30)  ;; Lower strike, long call
  (sell 1 call #:strike 150 #:expiration 30)) ;; Higher strike, short call

(define-option-strategy high-vol-strat
  #:ticker 'AAPL
  #:current-price 145.75
  #:safe-mode #t
  #:volatility 0.3  ;; 30% volatility
  #:risk-free-rate 0.02  ;; 2% risk-free rate
  (buy 1 call #:strike 140 #:expiration 30)
  (sell 1 call #:strike 150 #:expiration 30))

(define-option-strategy high-vol-strat-prem
  #:ticker 'AAPL
  #:current-price 280.75
  #:safe-mode #t
  #:volatility 0.3  ;; 30% volatility
  #:risk-free-rate 0.02  ;; 2% risk-free rate
  (buy 1 call #:strike 240 #:expiration 30 #:premium 7.50)
  (sell 1 call #:strike 320 #:expiration 30 #:premium 5.00))

(graph-multiple-strategies
 (list (list bullish-strat "Bull Call Spread" "blue")
       (list safe-strat "Covered Strangle" "green")
       (list high-vol-strat-prem "High Volatility" "red"))
 #:min-price 50  ; Optional manual price range
 #:max-price 350)

(define (≈ a b tol) (< (abs (- a b)) tol))

(define tol 0.001)

(define-test-suite premium-tests
  (check-true (≈ (calculate-premium 140 145.75 0.0822 0.02 0.3 'call) 8.4563 tol))
  (check-true (≈ (calculate-premium 150 145.75 0.0822 0.02 0.3 'call) 3.3159 tol))
  (check-true (≈ (calculate-premium 140 145.75 0.0822 0.02 0.3 'put) 2.4763 tol))
  (check-true (≈ (calculate-premium 150 145.75 0.0822 0.02 0.3 'put) 7.3195 tol))
  (check-true (≈ (calculate-premium 140 145.75 1 0.05 0.2 'call) 18.5054 tol))
  (check-true (≈ (calculate-premium 150 145.75 1 0.05 0.2 'put) 10.0195 tol)))

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

  ;; Expiration must be between 7-365 days
  (check-true (fails-to-compile?
               '(define-option-strategy risky-expiry
                  #:ticker 'AAPL
                  #:current-price 150
                  #:safe-mode #t
                  (buy 1 call #:strike 155 #:expiration 5)))  ;; Expiration < 7
               "Safe mode: Should fail because expiration is too short.")

  (check-true (fails-to-compile?
               '(define-option-strategy risky-expiry
                  #:ticker 'AAPL
                  #:current-price 150
                  #:safe-mode #t
                  (buy 1 put #:strike 140 #:expiration 400)))  ;; Expiration > 365
               "Safe mode: Should fail because expiration is too long.")

  ;; Cannot sell more contracts than total purchased (no over-leveraging)
  (check-true (fails-to-compile?
               '(define-option-strategy over-leveraged
                  #:ticker 'TSLA
                  #:current-price 700
                  #:safe-mode #t
                  (buy 2 call #:strike 750 #:expiration 30)
                  (sell 5 call #:strike 750 #:expiration 30)))  ;; Selling more than buying
               "Safe mode: Should fail because more options are sold than bought.")

  ;; Strike price must be within 30% of current price
  (check-true (fails-to-compile?
               '(define-option-strategy too-far-otm
                  #:ticker 'NFLX
                  #:current-price 500
                  #:safe-mode #t
                  (buy 1 call #:strike 700 #:expiration 60)))  ;; 40% away from current price
               "Safe mode: Should fail because strike price is too high.")

  (check-true (fails-to-compile?
               '(define-option-strategy too-far-itm
                  #:ticker 'NFLX
                  #:current-price 500
                  #:safe-mode #t
                  (buy 1 put #:strike 250 #:expiration 60)))  ;; Strike too far from current price
               "Safe mode: Should fail because strike price is too low.")

  ;; Naked short calls or puts are not allowed
  (check-true (fails-to-compile?
               '(define-option-strategy naked-call
                  #:ticker 'AMZN
                  #:current-price 3000
                  #:safe-mode #t
                  (sell 1 call #:strike 3100 #:expiration 30)))  ;; No corresponding "buy"
               "Safe mode: Should fail because selling a naked call.")

  (check-true (fails-to-compile?
               '(define-option-strategy naked-put
                  #:ticker 'GOOG
                  #:current-price 2800
                  #:safe-mode #t
                  (sell 1 put #:strike 2700 #:expiration 30)))  ;; No corresponding "buy"
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


#lang racket
(require (for-syntax syntax/parse)
         racket/date
         plot
         rackunit)

(begin-for-syntax
  (define-syntax-class action
    (pattern (~or buy sell)))
  
  (define-syntax-class option-type
    (pattern (~or call put)))

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
                       (~optional [~seq #:premium p:expr] #:defaults ([p #'#f]))) ...)
     
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


(require math/special-functions)

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
  #:current-price 145.75
  #:safe-mode #t
  #:volatility 0.3  ;; 30% volatility
  #:risk-free-rate 0.02  ;; 2% risk-free rate
  (buy 1 call #:strike 140 #:expiration 30 #:premium 5.00)
  (sell 1 call #:strike 150 #:expiration 30 #:premium 5.00))

(graph-multiple-strategies
 (list (list bullish-strat "Bull Call Spread" "blue")
       (list safe-strat "Covered Strangle" "green")
       (list high-vol-strat "High Volatility" "red"))
 #:min-price 50  ; Optional manual price range
 #:max-price 250)

(define (≈ a b tol) (< (abs (- a b)) tol))

(require rackunit/text-ui)

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

;; Run all tests with verbose output
(displayln "Running Premium Tests:")
(run-tests premium-tests 'verbose)
(newline)
(displayln "Running Payoff Tests:")
(run-tests payoff-tests 'verbose)


;; Example usage
#|
(define-option-strategy AAPL-strat
  #:ticker 'AAPL
  #:current-price 182.52
  #:safe-mode #t
  (sell  1 call  #:strike 200 #:expiration 20))
|#

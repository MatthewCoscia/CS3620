
# Options Trading

This trading DSL gives you the ability to test complex options trades before executing them with your broker. With a variety of trading presets and built-in error checking, the package helps ensure you're staying safe financially while trading.

## Example Program (3D Graphing)

```racket
#lang racket

(require trading)

(define collar-shortened
  (make-strategy/shortcut 'collar-shortened
                          #:ticker 'AAPL
                          #:ticker-price 145.75
                          `collar 140 150 30))

(define covered-call-test
  (make-strategy 'covered-call-test
                 #:ticker 'AAPL
                 #:ticker-price 150
                 (buy 100 shares)
                 (sell 1 call #:strike 160 #:expiration 30)))

(define (graph-preview-single)
  (graph-decision
   (list (list collar-shortened "Collar Spread" "blue")
         (list covered-call-test "Covered Call Spread" "green"))
   #:3d #t))
```

---

## Installing and Running

Clone the repository, change into the directory, and run:

```sh
raco pkg install
```

Then in your Racket code:

```racket
(require OptionsTradingDSL)
```

View documentation with:

```sh
raco docs OptionsTradingDSL
```

Run package tests with:

```sh
raco test -p OptionsTradingDSL
```

---

## Function Reference

### `graph-decision`

```racket
(graph-decision strategy-triplets
                #:3d [use-3d? #f]
                #:min-price [min-price #f]
                #:max-price [max-price #f]
                #:min-days [min-days 1]
                #:max-days [max-days #f]
                #:price-step [price-step 5]
                #:day-step [day-step 2]
                #:days-since-purchase [days-since #f])
```

Visualizes one or more strategies, optionally in 3D. Each `strategy-triplet` is a list:

```racket
(list strategy-name "Label" "Color")
```

**Optional keyword arguments:**

- `#:3d`: Enables 3D visualization if `#t`.
- `#:min-price`, `#:max-price`: Custom price bounds.
- `#:min-days`, `#:max-days`: Days-to-expiration bounds.
- `#:price-step`, `#:day-step`: Visualization resolution.
- `#:days-since-purchase`: Adjusts payoff view for elapsed time.

---

### `make-strategy`

```racket
(make-strategy name
               #:ticker ticker
               #:ticker-price cp
               #:volatility [vol 0.2]
               #:risk-free-rate [rfr 0.05]
               . legs)
```

Builds a strategy from individual legs (calls, puts, shares).

### `make-strategy/shortcut`

```racket
(make-strategy/shortcut name
                        #:ticker ticker
                        #:ticker-price cp
                        #:quantity [qty 1]
                        #:volatility [vol 0.2]
                        #:risk-free-rate [rfr 0.05]
                        strategy-type . args)
```

Creates a predefined strategy with simple arguments. Example strategy types include:

- `'call-debit-spread`
- `'put-debit-spread`
- `'butterfly-spread`
- `'call-credit-spread`
- `'put-credit-spread`
- `'iron-condor`
- `'iron-butterfly`
- `'long-straddle`
- `'long-strangle`
- `'covered-call`
- `'collar`
- `'diagonal-call-spread`

---

### `buy` / `sell`

```racket
(buy qty asset #:strike [strike #f] #:expiration [expiration #f] #:premium [premium #f])
(sell qty asset #:strike [strike #f] #:expiration [expiration #f] #:premium [premium #f])
```

**Arguments:**

- `qty`: Number of contracts/shares.
- `asset`: Either `shares`, `call`, or `put`.
- `#:strike`: Strike price (required for options).
- `#:expiration`: Days to expiration (options).
- `#:premium`: Optional cost per share.

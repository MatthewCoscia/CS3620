# Options Trading

This trading DSL gives you the abiltiy to test complex options trades before execution in your broker. With plenty of different option trading presets and error checking, the package makes sure you're staying safe financially while trading. 

Here's an example program that graphs in 3d

```
#lang racket

(require trading)

(define collar-shortened
  (make-strategy/shortcut 'collar-shortened
                          #:ticker 'AAPL
                          #:ticker-price 145.75
                          `collar 140 150 7))


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

## Installing and running

Check out this Git repository, change directory into it, and run:


```
raco pkg install
```

Then import as

```
(require trading)
```

Once installed, you can access the documentation via:

```
raco docs trading
```

Finally, you can run the tests with:

```
raco test -p trading
```

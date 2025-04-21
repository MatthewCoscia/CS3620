#lang scribble/manual

@(require (for-label racket OptionsTradingDSL))

@title{OptionsTradingDSL: A Library for Options Trading Strategy Analysis}

@defmodule[OptionsTradingDSL]

This library provides functions for creating, analyzing, and visualizing options trading strategies. It allows users to model complex option strategies and visualize their profit/loss profiles over time and price ranges.

@section{Creating Option Strategies}

@defproc[(make-strategy/shortcut [name any/c]
                                 [#:ticker ticker symbol?]
                                 [#:ticker-price cp positive?]
                                 [#:quantity qty positive-integer? 1]
                                 [#:volatility vol (>=/c 0) 0.2]
                                 [#:risk-free-rate rfr (>=/c 0) 0.05]
                                 [strategy-type any/c]
                                 [args any/c] ...)
         strategy?]{
 Creates an option strategy using a shortcut method for predefined strategy types.
 
 The @racket[name] parameter provides a descriptive name for the strategy.
 
 The @racket[ticker] parameter specifies the underlying security symbol.
 
 The @racket[cp] parameter (ticker-price) specifies the current price of the underlying security.
 
 The @racket[qty] parameter specifies the quantity of the strategy to create (default: 1).
 
 The @racket[vol] parameter specifies the implied volatility as a decimal (default: 0.2).
 
 The @racket[rfr] parameter specifies the risk-free interest rate as a decimal (default: 0.05).
 
 The @racket[strategy-type] and additional @racket[args] define the specific option strategy
 to create (e.g., spread types, strike prices, expirations).
 
 Returns a @racket[strategy] object representing the complete options strategy.
}

@defproc[(make-strategy [name any/c]
                        [#:ticker ticker symbol?]
                        [#:ticker-price cp positive?]
                        [#:volatility vol (>=/c 0) 0.2]
                        [#:risk-free-rate rfr (>=/c 0) 0.05]
                        [legs any/c] ...)
         strategy?]{
 Creates an option strategy by explicitly specifying each option leg.
 
 The @racket[name] parameter provides a descriptive name for the strategy.
 
 The @racket[ticker] parameter specifies the underlying security symbol.
 
 The @racket[cp] parameter (ticker-price) specifies the current price of the underlying security.
 
 The @racket[vol] parameter specifies the implied volatility as a decimal (default: 0.2).
 
 The @racket[rfr] parameter specifies the risk-free interest rate as a decimal (default: 0.05).
 
 The @racket[legs] parameter is a list of option legs, which can be created using helper
 functions like @racket[buy] and @racket[sell] (not shown in the provided code).
 
 Returns a @racket[strategy] object representing the complete options strategy.
}

@section{Visualizing Option Strategies}

@defproc[(graph-multiple-strategies-3d [strategies (listof (list/c strategy? any/c any/c))]
                                     [#:min-price min-price (or/c #f real?) #f]
                                     [#:max-price max-price (or/c #f real?) #f]
                                     [#:min-days min-days real? 1]
                                     [#:max-days max-days real? 60]
                                     [#:price-step price-step real? 5]
                                     [#:day-step day-step real? 2])
         void?]{
 Creates a 3D visualization of multiple option strategies showing profit/loss over
 both stock price and time dimensions.
 
 The @racket[strategies] parameter is a list of triplets, where each triplet contains:
 a strategy object, a label for the legend, and a color for the plot.
 
 The @racket[min-price] and @racket[max-price] parameters define the price range for the
 visualization. If not provided, appropriate bounds will be calculated.
 
 The @racket[min-days] and @racket[max-days] parameters define the time range (in days)
 for the visualization.
 
 The @racket[price-step] and @racket[day-step] parameters control the granularity of the
 visualization.
 
 Opens a new window with the 3D visualization.
}

@defproc[(graph-multiple-strategies-2d [strategies (listof (list/c strategy? any/c any/c))]
                                     [#:min-price min-price (or/c #f real?) #f]
                                     [#:max-price max-price (or/c #f real?) #f]
                                     [#:days-since-purchase days-since (or/c #f real?) #f])
         void?]{
 Creates a 2D visualization of multiple option strategies showing profit/loss profiles
 at a specific point in time.
 
 The @racket[strategies] parameter is a list of triplets, where each triplet contains:
 a strategy object, a label for the legend, and a color for the plot.
 
 The @racket[min-price] and @racket[max-price] parameters define the price range for the
 visualization. If not provided, appropriate bounds will be calculated.
 
 The @racket[days-since-purchase] parameter specifies the time point (in days since purchase)
 for which to display the profit/loss profile. If not provided, a default value will be used.
 
 Opens a new window with the 2D visualization.
}

@defproc[(graph-decision [strategy-triplets (listof (list/c strategy? any/c any/c))]
                       [#:3d use-3d? boolean? #f]
                       [#:min-price min-price (or/c #f real?) #f]
                       [#:max-price max-price (or/c #f real?) #f]
                       [#:min-days min-days real? 1]
                       [#:max-days max-days (or/c #f real?) #f]
                       [#:price-step price-step real? 5]
                       [#:day-step day-step real? 2]
                       [#:days-since-purchase days-since (or/c #f real?) #f])
         void?]{
 A convenience function that chooses between 2D and 3D visualizations based on the
 @racket[use-3d?] parameter.
 
 The @racket[strategy-triplets] parameter is a list of triplets, where each triplet contains:
 a strategy object, a label for the legend, and a color for the plot.
 
 The @racket[use-3d?] parameter determines whether to create a 3D visualization (when #t)
 or a 2D visualization (when #f).
 
 The remaining parameters are passed to either @racket[graph-multiple-strategies-3d] or
 @racket[graph-multiple-strategies-2d] based on the visualization type.
 
 If @racket[max-days] is not provided, it will be automatically calculated based on the
 maximum expiration date across all strategies.
 
 Opens a new window with the chosen visualization.
}

@section{Examples}

Here are some examples of using this library to create and visualize option strategies:

@codeblock{
#lang racket
(require OptionsTradingDSL)

;; Create a long call strategy
(define long-call
  (make-strategy/shortcut 'long-call
                        #:ticker 'AAPL
                        #:ticker-price 150
                        #:quantity 1
                        #:volatility 0.25
                        'long-call 160 30))

;; Create a bull call spread
(define bull-spread
  (make-strategy/shortcut 'bull-spread
                        #:ticker 'AAPL
                        #:ticker-price 150
                        #:quantity 1
                        #:volatility 0.25
                        'bull-call-spread 140 160 45))

;; Visualize these strategies
(graph-decision
  (list 
    (list long-call "Long Call" 'blue)
    (list bull-spread "Bull Call Spread" 'red))
  #:3d #t
  #:min-price 130
  #:max-price 180)
}

Note: The exact available strategy types and leg creation helpers (like 'long-call, 'bull-call-spread) 
are not fully documented in the provided code. Consult the implementation for all available options 
or refer to additional documentation.
;; ----------------------------------------
;; 1. Overview: Compilation Process
;; ----------------------------------------
;; This DSL compiles options trading strategies into structured Racket Macros.
;; The compilation process involves:
;; - **Parsing the DSL Syntax** using `syntax-parse` and syntax classes.
;; - **Performing static validation checks** (e.g., preventing naked shorts,
;; over-leveraging).
;; - **Transforming strategies into Racket data structures** for
;; runtime evaluation.
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
;; - `ACTION`: Either `buy` or `sell`, defining if the contract is bought or sold.
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
;; - **Expiration Range Check**: Options must expire in the future.
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

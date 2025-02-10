#lang racket

(require (for-syntax racket/base)
         (for-syntax syntax/parse))

;; A simple function that decides if `x` is a basic literal
;; (number, string, boolean, symbol, char, keyword, list, vector, etc.)
;; You can expand this if you want to handle more cases.
(define-for-syntax (my-literal? x)
  (or (boolean? x)
      (number? x)
      (symbol? x)
      (char? x)
      (string? x)
      (keyword? x)
      (list? x)
      (vector? x)
      (null? x)))

;; Error continuation for match failures
(define (error-on-fail)
  (error 'minimatch "all patterns failed to match"))

;; Inlined do-match macro
(define-syntax (do-match stx)
  (syntax-parse stx
    #:literals (cons quote)

    ;; Pattern: (quote val)
    [(_ target (quote val) success-body on-fail)
     #'(if (equal? target (quote val))
           success-body
           (on-fail))]

    ;; Pattern: variable identifier
    [(_ target id:id success-body on-fail)
     #'(let ([id target])
         success-body)]

    ;; Pattern: (cons pat1 pat2)
    [(_ target (cons pat1 pat2) success-body on-fail)
     #'(if (pair? target)
           (let ([car-val (car target)]
                 [cdr-val (cdr target)])
             (do-match car-val pat1
               (do-match cdr-val pat2 success-body on-fail)
               on-fail))
           (on-fail))]

    ;; Bare literal pattern (e.g. 5, "hello") that is not an identifier
    [(_ target pat success-body on-fail)
     #:when (and (my-literal? (syntax-e #'pat))
                 (not (identifier? #'pat)))
     #'(if (equal? target (quote pat))
           success-body
           (on-fail))]

    ;; Otherwise, error
    [(_ _ pat success-body on-fail)
     (raise-syntax-error 'do-match
       "unsupported pattern" #'pat)]))

;; -----------------------------------------------------------
;; Minimatch Macros for multi-clause matching
;; -----------------------------------------------------------

(define-syntax (minimatch stx)
  (syntax-parse stx
    [(_ target-expr [pat body] ...+)
     #'(let ([target-val target-expr])
         (match-clauses target-val ([pat body] ...)))]))

(define-syntax (match-clauses stx)
  (syntax-parse stx
    [(_ target-val ([pat body] [next-pat next-body] ...+))
     #'(do-match target-val pat body
         (lambda ()
           (match-clauses target-val ([next-pat next-body] ...))))]
    [(_ target-val ([pat body]))
     #'(do-match target-val pat body error-on-fail)]))

;; -----------------------------------------------------------
;; Tests
;; -----------------------------------------------------------
(module+ test
  (require rackunit)

  ;; Test bare-literal pattern
  (check-equal?
   (minimatch 5
     [5 'yes]
     [_ 'no])
   'yes)

  ;; Test variable binding
  (check-equal?
   (minimatch (cons 1 2)
     [(cons a b) (+ a b)])
   3)

  ;; Test nested cons patterns
  (check-equal?
   (minimatch (list 3 4 5)
     [(cons a (cons b (cons c '()))) (+ a b c)])
   12)

  ;; Test clause fallthrough
  (check-equal?
   (minimatch (list 1 2)
     [(cons 3 '()) 'no]
     [(cons a b) 'yes])
   'yes)

  ;; Test runtime failure
  (check-exn exn:fail?
    (lambda () (minimatch 42 [(cons a b) 'fail])))

  ;; Another bigger pattern
  (check-equal?
   (let ([v (cons (cons 1 2) (cons 3 4))])
     (minimatch v
       [(cons a '()) "one thing"]
       [(cons a (cons b '())) "two things"]
       [(cons (cons a b) (cons c d)) "binary tree"]))
   "binary tree")

  ;; Another example with a quoted literal
  (check-equal?
   (let ()
     (define (f x)
       (minimatch x
         [(cons '1 b) (* b 2)]))
     (f (cons 1 2)))
   4))

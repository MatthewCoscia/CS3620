#lang racket

(require "runtime.rkt"
         (for-syntax syntax/parse racket/syntax racket/set racket/list))

(define-for-syntax (get-bound-vars pat)
  "Returns a list of identifiers bound in the given pattern."
  (syntax-parse pat
    #:literals (cons list and or quote)
    [(quote _) '()]
    [id:id (list #'id)]
    [(cons p1 p2)
     (append (get-bound-vars #'p1) (get-bound-vars #'p2))]
    [(list ps ...)
     (apply append (map get-bound-vars (syntax->list #'(ps ...))))]
    [(and ps ...)
     (apply append (map get-bound-vars (syntax->list #'(ps ...))))]
    [(or p ...)
     (apply append (map get-bound-vars (syntax->list #'(p ...))))]
    [_ (raise-syntax-error 'get-bound-vars "unsupported pattern" pat)]))

;;----------------------------------------------------------------
;; do-match Macro
;;
;; Usage:
;;   (do-match target-id pat success-body-expr on-fail-id)
;;
;; Grammar for patterns (<pat>):
;;   - (quote <datum>)   ; literal pattern (written as 'literal)
;;   - <id>              ; a pattern variable
;;   - (cons <pat> <pat>) ; a cons pattern
;;
;; For example, a match like:
;;   (let ([x (cons 1 2)])
;;     (match x
;;       [(cons a b) (+ a b)]))
;; will be written as:
;;   (let ([x (cons 1 2)])
;;     (do-match x (cons a b) (+ a b) error-on-fail))
;;
(define-syntax (do-match stx)
  (syntax-parse stx
    #:literals (cons quote list and or)

    ;; Literal pattern (quote <datum>)
    [(_ target (quote x) success-body on-fail)
     #'(match-== target (quote x)
                 (lambda () success-body)
                 on-fail)]

    ;; Variable pattern (single identifier)
    [(_ target id:identifier success-body on-fail)
     #'(match-var target
                  (lambda (dummy)
                    (let ([id dummy]) success-body))
                  on-fail)]

    ;; Cons pattern (cons <pat1> <pat2>)
    [(_ target (cons p1 p2) success-body on-fail)
     #'(match-cons target
                   (lambda (car-val cdr-val)
                     (do-match car-val p1
                               (do-match cdr-val p2 success-body on-fail)
                               on-fail))
                   on-fail)]

    ;; Empty list pattern (list)
    [(_ target (list) success-body on-fail)
     #'(match-== target '()
                 (lambda () success-body)
                 on-fail)]

    ;; List pattern (list <pat> ...)
    [(_ target (list pat ...+) success-body on-fail)
     (with-syntax ([cons-pattern
                    (foldr (lambda (p acc)
                             #`(cons #,p #,acc))
                           ''()
                           (syntax->list #'(pat ...)))])
       #'(do-match target cons-pattern success-body on-fail))]

    ;; And pattern (and <pat1> <pat2> ...)
    [(_ target (and p1 p2 ...+) success-body on-fail)
     (foldr (lambda (p acc)
              #`(do-match target #,p #,acc on-fail))
            #'success-body
            (syntax->list #'(p1 p2 ...)))]

    ;; Or pattern (or <pat1> <pat2> ...)
    [(_ target (or p1 p2 ...+) success-body on-fail)
     ;; 1) Extract branches and their variables
     (define p1-syn #'p1)
     (define p2-syn-list (syntax->list #'(p2 ...)))
     (define p1-vars (get-bound-vars p1-syn))

     ;; Helper to convert variables to sorted symbols
     (define (vars->sorted-symbols vars)
       (sort (remove-duplicates (map syntax-e vars)) symbol<?))

     ;; Get symbols from the first branch
     (define p1-syms (vars->sorted-symbols p1-vars))

     ;; 2) Validate other branches
     (for ([branch (in-list p2-syn-list)])
       (define branch-vars (get-bound-vars branch))
       (define branch-syms (vars->sorted-symbols branch-vars))
       (unless (equal? p1-syms branch-syms)
         (raise-syntax-error 'do-match
                             "all branches of an `or` pattern must bind the same set of variables"
                             branch)))

     ;; 3) Build nested or-chain
     (define nested
       (foldr (lambda (this-pat acc)
                #`(do-match target #,this-pat
                            (match-success #,@p1-vars)
                            #,acc))
              #'on-fail
              p2-syn-list))

     ;; 4) Final syntax
     #`(let ([match-success (lambda (#,@p1-vars) success-body)])
         (do-match target #,p1-syn
                   (match-success #,@p1-vars)
                   #,nested))]

    ;; Malformed cons patterns
    [(_ _ (cons . elems) success-body on-fail)
     (raise-syntax-error 'do-match
                         "malformed cons pattern, expected (cons <pat> <pat>)"
                         #'(cons . elems))]

    ;; Malformed patterns (fallback)
    [(_ _ pat success-body on-fail)
     (raise-syntax-error 'do-match
                         "unsupported pattern"
                         #'pat)]))

(module+ test
  (require rackunit)

  ;; Simple Or Pattern
  (check-equal?
   (minimatch (cons (cons 1 2) 3)
              [(or (cons (cons a b) c) (cons a (cons b c)))
               (list a b c)])
   '(1 2 3))

  #;
  ;; Or Pattern with Two Matching Branches (should take first)
  (check-equal?
   (minimatch '(42)
              [(or (list a) (cons a b)) a])
   42)

  #;
  ;; Or Pattern with a Failing Branch
  (check-equal?
   (minimatch (cons 5 10)
              [(or (list a b) (cons a b)) (list a b)]))


  #;
  ;; Or Pattern Fails (No Matches)
  (check-exn
   exn:fail?
   (lambda ()
     (minimatch 42
                [(or (list a b) (cons a b)) a])))

  #;
  ;; More Than Two Patterns in `or`
  (check-equal?
   (minimatch '(10 20)
              [(or (list a b) (list a b) (list a b c))
               (list a b)]))

  #;
  ;; Ensuring `or` Branches Bind the Same Variables
  (check-exn
   exn:fail?
   (lambda ()
     (minimatch 5
                [(or (list a b) x) x])))
  )


(module+ test
  (require rackunit)

  ;; Simple and-pattern matching
  (check-equal?
   (minimatch '(1 2)
              [(and l (list a b))
               (format "the list ~a has elements ~a and ~a" l a b)])
   "the list (1 2) has elements 1 and 2")
  ;; And-pattern with a cons structure
  (check-equal?
   (minimatch (cons 5 10)
              [(and x (cons a b)) (list x a b)])
   '((5 . 10) 5 10))  ;; Should return the full matched value and elements.

  ;; And-pattern with a quoted literal
  (check-equal?
   (minimatch 42
              [(and x '42) x])
   42)

  ;; Failure case (one pattern fails)
  (check-exn
   exn:fail?
   (lambda ()
     (minimatch 5 [(and (list a b) x) x])))

  ;; More than two sub-patterns in `and`
  (check-equal?
   (minimatch '(10 20)
              [(and l (list a b) (cons x y))
               (list l a b x y)])
   '((10 20) 10 20 10 (20)))

  ;; Failure case (not matching all patterns)
  (check-exn
   exn:fail?
   (lambda ()
     (minimatch '(5) [(and (list a b) x) x]))))


(module+ test
  (require rackunit)

  ;; Test matching a single-element list
  (check-equal?
   (minimatch '(42)
              [(list a) a]
              [_ 'fail])
   42)

  ;; Test matching a two-element list
  (check-equal?
   (minimatch '(1 2)
              [(list a) "one"]
              [(list a b) "two"]
              [_ "fail"])
   "two")

  ;; Test matching a three-element list
  (check-equal?
   (minimatch '(1 2 3)
              [(list a) "one"]
              [(list a b) "two"]
              [(list a b c) "binary"]
              [_ "fail"])
   "binary")

  ;; Test failure case (list size mismatch)
  (check-exn
   exn:fail?
   (lambda () (minimatch '(1 2 3 4)
                         [(list a b c) "binary"])))

  ;; Test empty list match
  (check-equal?
   (minimatch '()
              [(list) "empty"]
              [_ "fail"])
   "empty"))


(module+ test
  (require rackunit
           syntax/macro-testing)

  ;; A helper failure continuation for testing.
  (define test-fail
    (lambda () (error 'minimatch "match failure")))
  
  ;; Test 1: Successful match using a cons pattern.
  (define x (cons 1 2))
  (check-equal? (do-match x (cons a b) (+ a b) error-on-fail) 3)
  
  ;; Test 2: Correct expected value to 10
  (check-equal? (do-match 1 '1 (+ 10 0) error-on-fail) 10)
  
  ;; Test 3: Runtime failure when matching a non-pair with a cons pattern.
  (check-exn
   #rx"minimatch: all patterns failed to match"
   (lambda ()
     (do-match 42 (cons a b) (+ a b) error-on-fail)))
  
  ;; Test 4: Malformed pattern using convert-compile-time-error
  (check-exn
   #rx"malformed cons pattern"
   (lambda ()
     (convert-compile-time-error
      (do-match x (cons a) (+ a 1) error-on-fail)))))

(define-syntax (minimatch stx)
  (syntax-parse stx
    [(_ target-expr [pat body] ...+)
     #'(let ([target-val target-expr])
         (minimatch-val target-val [pat body] ...))]))

(define-syntax (minimatch-val stx)
  (syntax-parse stx
    [(_ target-val [pat body] ...+)
     #'(match-clauses target-val ([pat body] ...))]))

(define-syntax (match stx)
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

(module+ test
  (check-equal?
   (let ([v (cons (cons 1 2) (cons 3 4))])
     (minimatch v
                [(cons a '()) "one thing"]
                [(cons a (cons b '())) "two things"]
                [(cons (cons a b) (cons c d)) "binary tree"]))
   "binary tree")

  ;; Test multiple clauses with earlier failures
  (check-equal?
   (minimatch (list 1 2)
              [(cons a '()) "one"]
              [(cons a (cons b '())) "two"]
              [_ "other"])
   "two")

  ;; Test all clauses fail
  (check-exn
   #rx"minimatch: all patterns failed to match"
   (lambda ()
     (minimatch 5
                [(cons a b) 'fail1]
                ['x 'fail2])))
  
  ;; 6.3.1 Example Expansion Test
  (check-equal?
   (let ([v (cons (cons 1 2) (cons 3 4))])
     (let ([on-clause1-fail (lambda ()
                              (let ([on-clause2-fail (lambda ()
                                                       (do-match v (cons (cons a b) (cons c d))
                                                                 "binary tree"
                                                                 error-on-fail))])
                                (do-match v (cons a (cons b '()))
                                          "two things"
                                          on-clause2-fail)))])
       (do-match v (cons a '())
                 "one thing"
                 on-clause1-fail)))
   "binary tree"))



(provide do-match minimatch)
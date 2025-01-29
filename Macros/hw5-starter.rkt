#lang racket

(provide swap! for-list and/as cases)

(require (for-syntax syntax/parse))

(module+ test
  (require rackunit)
  (require syntax/macro-testing))

;; ---------------------------------------------------------------------------------------------------
;; (swap! <id> <id>)
;;
;; Mutates the variable named by the first <id> to contain the original value and vice versa.
;;
;; Example:
#;
(let ([x 5]
      [y 6])
  (swap! x y)
  (list x y))
;; Evaluates to:
#;
'(6 5)

;; Example expansion:
;; (let ([tmp x])
;;   (set! x y)
;;   (set! y tmp))
;; where tmp is a generated unique identifier.

(define-syntax (swap! stx)
  (syntax-parse stx
    [(_ id1:id id2:id)
     #'(let ([tmp id1])
         (set! id1 id2)
         (set! id2 tmp))]
    [(_ . _)
     (raise-syntax-error 'swap! "expected two identifiers" stx)]))


(module+ test
  (test-case "swap! exchanges values"
    (let ([x 5] [y 6])
      (swap! x y)
      (check-equal? (list x y) '(6 5))))
  
  (test-case "swap! same values"
    (let ([a 10] [b 10])
      (swap! a b)
      (check-equal? a 10)
      (check-equal? b 10)))
  
  (test-case "swap! same variable"
    (let ([x 7])
      (swap! x x)
      (check-equal? x 7)))

  (test-case "swap! fails with three identifiers"
    (check-exn
      exn:fail:syntax?
      (lambda ()
        (eval '(swap! x y z))) ; Quote the invalid macro call
      "swap! should fail when given three identifiers"))

  
  (test-case "swap! with different types"
    (let ([a "hello"] [b 42])
      (swap! a b)
      (check-equal? a 42)
      (check-equal? b "hello"))))

;; ---------------------------------------------------------------------------------------------------
;; (for-list (<binding> ...+) <expr>)
;;
;; <binding> := [<id> <expr>]
;;
;; Simultaneously iterates over the lists provided as the <expr>s of the
;; <binding> groups. For each element of the lists, evaluates the body <expr> with the
;; <id>s bound to the values in the lists and collects the results of these evaluation
;; in the output list.
;;
;; Raises an error if the lists are not the same length. (Note that this is a different
;; than Racket's `for/list`)
;;
;; Example:
#;
(for-list ([char '(#\a #\b #\c)]
           [n '(1 2 3)])
          (make-string n char))
;; Evaluates to:
#;
'("a" "bb" "ccc")

;; Example expansion:
;; TODO
;;
;; Hint: expand to a use of `map`.

(define-syntax for-list
  (lambda (stx)
    (syntax-parse stx
      [(_ ([id:identifier e:expr] ...+) body:expr)
       #`(let ([lists (list e ...)])
           (unless (apply = (map length lists))
             (error 'for-list "all lists must have the same length"))
           (apply map 
                  (lambda (id ...) 
                    body)
                  lists))])))

(module+ test
  
  (test-case "for-list basic functionality"
    (check-equal?
     (for-list ([char '(#\a #\b #\c)]
                [n '(1 2 3)])
       (make-string n char))
     '("a" "bb" "ccc")
     "basic character/string conversion")

    (check-equal?
     (for-list ([a '(1 2 3)] [b '(4 5 6)]) (+ a b))
     '(5 7 9)
     "numeric computation with two lists")))

;; ---------------------------------------------------------------------------------------------------
;; (and/as <clauses>)
;; 
;; <clauses> := <expr>
;;            | <expr> #:as <id> <clauses>
;;            | <expr> <clauses>
;;
;; Like Racket's `and`, but after any but the last expression there may be
;; an #:as <id>. Then, the value of the preceding expression is bound as that
;; name for the evaluation of the subsequent expressions.
;;
;; Like in Racket's `and`, if one of the <expr>s evaluates to #f, the following
;; expressions should not be evaluated.
;;
;; Example:
#;
(define fiona (hash 'name "Fiona Brown" 'articles 3))
#;
(and/as (hash-ref fiona 'articles #f) #:as count
        (number? count)
        (= count 3))
;; Evaluates to:
#;
#t

;; Example expansion:
;;TODO
;;
;; Hint: expand to uses of `let` and `and`.

(define-syntax and/as
  (lambda (stx)
    (syntax-parse stx
      [(_) #'#t]
      [(_ expr) #'expr]
      [(_ expr #:as id rest ...)
       #'(let ([id expr])
           (and id (and/as rest ...)))]
      [(_ expr rest ...)
       #'(and expr (and/as rest ...))])))

(module+ test
  
  (test-case "and/as with hash binding example"
    (let ([fiona (hash 'name "Fiona Brown" 'articles 3)])
      (check-true (and/as (hash-ref fiona 'articles #f) #:as count
                          (number? count)
                          (= count 3)))))
  (test-case "Empty returns true"
    (check-true (and/as)))
  )

;; ---------------------------------------------------------------------------------------------------
;; Original grammar:
;;
;; (cases <expr>
;;   <clause> ...
;;   <maybe-else-clause>)
;;
;; <clause> := [(<datum> ...+) <expr>]
;;
;; <maybe-else-clause> :=
;;                      | [#:else <expr>]
;;
;; Evaluates the <expr> and uses the result to select a clause. The selected clause is
;; the first one with a <datum> whose quoted form is equal? to the result of the initial <expr>.
;; If no such datum is present, the else clause is selected. If no else clause is present, an
;; error is raised. The result is the value produced by evaluating the selected clause's <expr>.
;;
;; Example:
#;
(define (categorize-animal animal)
  (cases animal
    [(cat dog cow) 'mammal]
    [(sparrow pigeon eagle) 'bird]
    [#:else 'unknown]))
#;
(categorize-animal 'dog)
;; Evaluates to:
#;
'mammal

#;
(categorize-animal 'turtle)
;; Evaluates to:
#;
'unknown
;;
;; Example expansion (of categorize-animal):
;; TODO
;;
;; Hint: Expand to nested `if`s that use `member`. You will need to transform
;; the grammar to another form that will correspond to your syntax-parse pattern
;; matches, like we did in class for `cond`. Write the transformed grammar below.

;; Grammar transformed for pattern matching:
;;   TODO
(define-syntax cases
  (lambda (stx)
    (syntax-parse stx
      [(_ val-expr
          [(datum ...+) body] ...
          (~optional [#:else else-expr]))
       #`(let ([val val-expr])
           #,(let loop ([datums (syntax->list #'((datum ...) ...))]
                         [bodies (syntax->list #'(body ...))])
               (if (null? datums)
                   (if (attribute else-expr)
                       #'else-expr
                       #'(error 'cases "no clause matched and no else clause"))
                   #`(if (member val (quote #,(car datums)))
                         #,(car bodies)
                         #,(loop (cdr datums) (cdr bodies))))))])))



(define (categorize-animal animal)
  (cases animal
    [(cat dog cow) 'mammal]
    [(sparrow pigeon eagle) 'bird]
    [#:else 'unknown]))

(module+ test
  (test-case "Categorize known animals"
    (check-equal? (categorize-animal 'dog) 'mammal)
    (check-equal? (categorize-animal 'cow) 'mammal)
    (check-equal? (categorize-animal 'sparrow) 'bird)
    (check-equal? (categorize-animal 'eagle) 'bird))

  (test-case "Categorize unknown animal"
    (check-equal? (categorize-animal 'turtle) 'unknown))

  (test-case "Cases with explicit else clause"
    (check-equal? (cases 'apple [(apple orange banana) 'fruit] [#:else 'not-fruit]) 'fruit)
    (check-equal? (cases 'grape [(apple orange banana) 'fruit] [#:else 'not-fruit]) 'not-fruit))

  (test-case "Error when no matching clause and no else clause"
    (check-exn exn:fail? (λ () (cases 'x [(a b c) 'matched])) "Should raise error when no matching clause and no else clause"))

  (test-case "Cases with a single match"
    (check-equal? (cases 'yes [(yes) 'affirmative] [(no) 'negative] [#:else 'neutral]) 'affirmative))
)
          
          


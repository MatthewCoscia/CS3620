#lang racket
(require syntax-spec-v3
         syntax/parse/define
         (for-syntax syntax/parse syntax-spec-v3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core DSL definition using syntax-spec

(syntax-spec
 ;; Binding class for pattern variables
 (binding-class racket-var #:binding-space pattern-binding)

 ;; Pattern nonterminal with explicit exports
 (nonterminal/exporting pat
                        #:binding-space pattern-binding
                        #:allow-extension (pattern-macro)
                        var:racket-var
                        #:binding (export var)
                        (== e:racket-expr)
                        #:binding ()
                        (cons p:pat q:pat)
                        #:binding [(re-export p) (re-export q)])

 ;; Extension class for pattern macros
 (extension-class pattern-macro
                  #:binding-space pattern-binding)

 ;; Host interface: compile macromatch to minimatch
 (host-interface/expression
  (macromatch e:racket-expr [p:pat e_body:racket-expr] ...)
  #:binding (scope (import p) ... e_body ...)
  #`(begin
      (printf "Compiling macromatch: ~a\n" (syntax->datum #'(macromatch e [p e_body] ...)))
      (minimatch e [p e_body] ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern Macros using define-dsl-syntax

(begin-for-syntax
  ;; Syntax class for literal data
  (define-syntax-class datum
    (pattern d #:when (not (identifier? #'d)))))

;; Macros that expand to core patterns
(define-dsl-syntax quote-pat pattern-macro
  (syntax-parser
    [(_ d:datum)
     #'(== 'd)]))

(define-dsl-syntax list-pat pattern-macro
  (syntax-parser
    [(_)
     #'(== '())]
    [(_ first)
     #`(cons first (== '()))]
    [(_ first second)
     #`(cons first (cons second (== '())))]
    [(_ first second rest ... )
     #`(cons first (cons second (list-pat rest ...)))]))



(define-dsl-syntax quasiquote pattern-macro
  (syntax-parser
    [(_ qq)
     (define (transform stx)
       (syntax-parse stx
         #:datum-literals (unquote)
         [(unquote x:id) #'x]
         [(a . b)
          (with-syntax ([transformed-a (transform #'a)]
                        [transformed-b (transform #'b)])
            #'(cons transformed-a transformed-b))]
         [() #'(== '())]
         [lit
          (let ([lit-val (syntax->datum #'lit)])
            #`(== '#,lit-val))]))
     (transform #'qq)]))








(begin-for-syntax
  (define (inspect-binding-space stx)
    (printf "Inspecting: ~a\n" (syntax->datum stx))
    (printf "Binding space marks: ~a\n" (syntax-debug-info stx))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiler to minimatch (using provided inlined.rkt)

(define-syntax minimatch
  (lambda (stx)
    (syntax-parse stx
      [(_ target clause ...)
       #'(let ([v target])
           (minimatch-val v clause ...))])))

(define-syntax minimatch-val
  (lambda (stx)
    (syntax-parse stx
      [(_ target:id)
       #'(error 'minimatch "no matching clause")]
      [(_ target:id [pat body] clause ...)
       #'(let ([fail (λ () (minimatch-val target clause ...))])
           (do-match target pat body fail))])))

(define-syntax do-match
  (lambda (stx)
    (syntax-parse stx
      [(do-match target pat body fail)
       (printf "do-match received: ~a\n" (syntax->datum #'pat)) ; For debugging
       (syntax-parse #'pat
         #:datum-literals (cons ==)  ; Match cons and == by symbolic name
         [(cons p1 p2)
          #'(if (pair? target)
                (let ([car-v (car target)]
                      [cdr-v (cdr target)])
                  (do-match car-v p1
                            (do-match cdr-v p2 body fail)
                            fail))
                (fail))]
         [(== e)
          #'(if (equal? target e) body (fail))]
         [x:id
          #'(let ([x target]) body)])])))

(begin-for-syntax
  (define (quasiquote-pat-transform stx)
    (syntax-parse stx
      #:datum-literals (unquote)
      [(unquote x:id)
       #'x]
      [(a . b)
       #`(cons ,(quasiquote-pat-transform #'a)
               ,(quasiquote-pat-transform #'b))]
      [()
       #'(== '())]
      [lit
       #`(== '#,stx)])))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require rackunit)



  (begin-for-syntax
    (define (safe-transform stx)
      (with-handlers ([exn:fail? 
                       (lambda (exn)
                         (displayln (format "Caught error: ~a" (exn-message exn)))
                         #'(error "partial result"))])
        (quasiquote-pat-transform stx)))  ; suppose you’ve factored out your transform
    )

  (define qq-transformed
    (expand-syntax #'(quasiquote-pat (1 (my-unquote a)))))
  (printf "quasiquote-pat expansion: ~a\n" (syntax->datum qq-transformed))


  (expand-syntax #'(quasiquote-pat (1 (quote (unquote a)))))

  ;; Quote patterns
  (define (g1 v)
    (macromatch v
                [(quote-pat (1))   "one thing"]
                [(quote-pat (1 2)) "two things"]))

  (check-equal? (g1 '(1))   "one thing")
  (check-equal? (g1 '(1 2)) "two things")

  ;; List Patterns
  (define (g2 v)
    (macromatch v
                [(list-pat a)   "matched one"]
                [(list-pat c b) "matched two"]))


  (check-equal? (g2 '(1))   "matched one")
  (check-equal? (g2 '(1 2)) "matched two")

  (define (g3 v)
    (macromatch v
                [`(1 ,a)   "matched one"]
                [`(2 ,b)   "matched two"]
                [`(,x ,y)  "matched any two-element list"]))

  (check-equal? (g3 '(1 10))   "matched one")
  (check-equal? (g3 '(2 20))   "matched two")
  (check-equal? (g3 '(5 6))    "matched any two-element list")

  )

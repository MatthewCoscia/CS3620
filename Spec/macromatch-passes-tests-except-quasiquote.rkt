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
                         ()
                         #:binding ()
                         (== e:racket-expr)
                         #:binding ()
                         (cons p:pat q:pat)
                         #:binding [(re-export p) (re-export q)]
                         (quasiquote p:pat)
                         #:binding [(re-export p)]
                         #:allow-extension pattern-macro)

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



(define-dsl-syntax quasiquote-pat pattern-macro
  (syntax-parser
    [(_ qq)
     (printf "Expanding quasiquote-pat: ~a\n" (syntax->datum #'qq))
     (define (transform stx)
       (syntax-parse stx
         #:datum-literals (unquote)
         [(unquote x:id)
          (begin
            (printf "Transforming unquote: ~a\n" (syntax->datum #'x))
            (define x* (datum->syntax stx (syntax-e #'x) stx stx))
            (printf "After introduce: ~a\n" (syntax->datum x*))
            x*)]
         [(a . b)
          (begin
            (printf "Transforming pair: ~a\n" (syntax->datum #'(a . b)))
            (datum->syntax
             stx
             (list 'cons (transform #'a) (transform #'b))
             stx))]
         [() 
          (printf "Transforming empty list\n")
          #'(== '())]
         [lit #:when (not (identifier? #'lit))
          (printf "Transforming literal: ~a\n" (syntax->datum #'lit))
          #'(== 'lit)]))
     (define expanded (transform #'qq))
     (printf "Final expansion of quasiquote-pat: ~a\n" (syntax->datum expanded))
     #`(quasiquote ,expanded)
     ]))



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
       (printf "do-match received: ~a\n" (syntax->datum #'pat))
       (syntax-parse #'pat
         #:datum-literals (cons ==)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(module+ test
  (require rackunit)

  ;; Quote patterns
  (define (g1 v)
    (macromatch v
                [(quote-pat (1))   "one thing"]
                [(quote-pat (1 2)) "two things"]))

  (check-equal? (g1 '(1))   "one thing")
  (check-equal? (g1 '(1 2)) "two things")

  (define (g2 v)
    (macromatch v
                [(list-pat a)   "matched one"]
                [(list-pat c b) "matched two"]))


  (check-equal? (g2 '(1))   "matched one")
  (check-equal? (g2 '(1 2)) "matched two")

  ;; Doesn't work....
  #;
  (define (g3 v)
    (macromatch v
                [`(1 ,a)   "matched one"]
                [`(2 ,b)   "matched two"]
                [`(,x ,y)  "matched any two-element list"]))

  #;
  (check-equal? (g3 '(1 10))   "matched one")
  #;
  (check-equal? (g3 '(2 20))   "matched two")
  #;
  (check-equal? (g3 '(5 6))    "matched any two-element list")

  )

#lang racket
(require syntax-spec-v3
         syntax/parse
         (for-syntax syntax/parse syntax/stx racket/match racket/base))
(provide router)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DSL Definition using syntax-spec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(syntax-spec
  (binding-class route-var #:binding-space route-binding)

  (nonterminal http-method
    get
    post
    put
    delete)

  (nonterminal/exporting path-segment
                         #:binding-space route-binding
                         id:identifier
                         (string-parameter var:route-var) #:binding (export var)
                         (number-parameter var:route-var) #:binding (export var))

  (nonterminal/exporting path
                         #:binding-space route-binding
                         (ps:path-segment ...)
                         #:binding [(re-export ps) ...]) 

  (nonterminal route
               #:binding-space route-binding
               (method:http-method [p:path] action:racket-expr)
               #:binding (scope (import p) action))


  (nonterminal routes
               #:binding-space route-binding
               ()
               (r:route rs:routes ...))
  

  (host-interface/expression
   (router method-expr:racket-expr path-expr:racket-expr rs:routes)
   #:binding ()
   (begin
     (check-unreachable-routes! (syntax->list #'rs))
     (compile-routes #'rs #'method-expr #'path-expr)))

  )


(begin-for-syntax
  (define (same-length? lst1 lst2)
    (cond
      [(and (null? lst1) (null? lst2)) #t]
      [(or (null? lst1) (null? lst2)) #f]
      [else (same-length? (cdr lst1) (cdr lst2))]))

  (define (check-unreachable-routes! routes)
    (define unreachable (first-unreachable-route routes))
    (when unreachable
      (raise-syntax-error 'router "unreachable route" unreachable)))

  (define (first-unreachable-route routes)
    (printf "Analyzing routes:\n")
    (for ([route (in-list routes)])
      (printf "  Route: ~s\n" (syntax->datum route)))
    (let loop ([prev-routes '()] [remaining routes])
      (if (null? remaining)
          #f
          (let* ([current (car remaining)]
                 [current-info (analyze-route current)]
                 [unreachable? (ormap (λ (prev)
                                       (route-superset? (analyze-route prev) current-info))
                                     prev-routes)])
            (printf "Checking route: ~s\n" (syntax->datum current))
            (printf "Unreachable? ~a\n" unreachable?)
            (if unreachable?
                current
                (loop (cons current prev-routes) (cdr remaining)))))))

  (struct route-info (method path-segments) #:transparent)

  (define (extract-path-segments path-stx)
    (map analyze-segment (syntax->list path-stx)))

  (define (analyze-route stx)
  (syntax-parse stx #:datum-literals (get post put delete)
    [(method [p ...] action)
     (route-info (syntax->datum #'method)
                 (map analyze-segment (syntax->list #'(p ...))))]))
  

  (define (analyze-segment seg)
    (syntax-parse seg #:datum-literals (string-parameter number-parameter)
      [id:identifier
       (cons 'literal (symbol->string (syntax-e #'id)))]
      [(string-parameter _)
       (cons 'string-param #f)]
      [(number-parameter _)
       (cons 'number-param #f)]))

  (define (route-superset? prev current)
    (printf "Comparing routes:\n  Prev: ~s\n  Current: ~s\n"
            (route-info-path-segments prev)
            (route-info-path-segments current))
    (and (equal? (route-info-method prev) (route-info-method current))
         (same-length? (route-info-path-segments prev)
                       (route-info-path-segments current))
         (for/and ([p-seg (in-list (route-info-path-segments prev))]
                   [c-seg (in-list (route-info-path-segments current))])
           (segment-superset? p-seg c-seg))))

  (define (segment-superset? prev-seg current-seg)
  (match* (prev-seg current-seg)
    [((cons 'string-param _) _) #t]
    [((cons 'number-param _) (cons 'number-param _)) #t]
    [((cons 'literal p-lit) (cons 'literal c-lit))
     (equal? p-lit c-lit)]
    [((cons 'number-param _) (cons 'literal c-lit))
     (number? (string->number c-lit))]
    [(_ _) #f]))

  (define (compile-routes routes-stx method-expr path-expr)
    #'(match (list #,method-expr #,path-expr)
        #,@(compile-routes-clauses routes-stx)
        [_ (error 'router "no matching route")]))

(define (compile-routes-clauses routes)
    (for/list ([route (in-list (syntax->list routes))])
      (syntax-parse route #:datum-literals (get post put delete string-parameter number-parameter)
        [(method [seg ...] action)
         (define compiled-segs
           (for/list ([seg (in-list (syntax->list #'(seg ...)))])
             (syntax-parse seg #:datum-literals (string-parameter number-parameter)
               [id:identifier
                #`#,(symbol->string (syntax-e #'id))]
               [(string-parameter var)
                (datum->syntax #f (syntax-e #'var))]
               [(number-parameter var)
                #`(app string->number (? number? #,(datum->syntax #f (syntax-e #'var))))])))
         #`[(list (quote #,(syntax->datum #'method)) (list #,@compiled-segs))
            action]])))


  (define (compile-path-pattern segs)
  (define compiled-segs
    (for/list ([seg (in-list (syntax->list segs))])
      (syntax-parse seg
        [id:identifier
         #`(== #,(symbol->string (syntax-e #'id)))]
        [(string-parameter var)
         (datum->syntax #f (syntax-e #'var))]
        [(number-parameter var)
         #`(app string->number (? number? #,(datum->syntax #f (syntax-e #'var))))])))
  #`(list #,@compiled-segs))
  
  )


(module+ test
  (require rackunit syntax/macro-testing)

  (test-case "Single route expands without error"
    (check-not-exn
     (lambda ()
       (expand-syntax
        #'(router get '("blog")
                  ((get [blog] 'list-articles)))))))

  (test-case "Multiple routes expands without error"
    (check-not-exn
     (lambda ()
       (expand-syntax
        #'(router get '("blog")
                  ((get [blog] 'list-articles))
                  ((get [blog (number-parameter article-no)]
                        (list 'numbered article-no))))))))

  (test-case "Single route expands correctly"
    (check-equal?
     (syntax->datum
      (expand-syntax
       #'(router get '("blog")
                 ((get [blog] 'list-articles)))))
     '(match (list 'get '("blog"))
             ((list 'get (list "blog")) 'list-articles)
             (_ (error 'router "no matching route")))))

  (test-case "Multiple routes expands correctly"
    (check-equal?
     (syntax->datum
      (expand-syntax
       #'(router get '("blog")
                 ((get [blog] 'list-articles))
                 ((get [blog (number-parameter article-no)]
                       (list 'numbered article-no))))))
     '(match (list 'get '("blog"))
             ((list 'get (list "blog")) 'list-articles)
             ((list 'get (list "blog" (app string->number (? number? article-no))))
              (list 'numbered article-no))
             (_ (error 'router "no matching route"))))))




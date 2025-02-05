#lang racket
; A FailureK is a (-> Any)
 
; FailureK
(define (error-on-fail)
  (error 'minimatch "all patterns failed to match"))
 
; ——————————————————-
; Any Any (-> Any) FailureK -> Any
; Matches values that are equal? to val. The success
; continuation is called with no arguments.
(define (match-== target val on-success on-fail)
  (if (equal? target val)
      (on-success)
      (on-fail)))

(module+ test
  (require rackunit)

  ;; Test when the target equals the given value.
  (check-equal? 
    (match-== 42 42 
               (lambda () 'matched)
               (lambda () 'failed))
    'matched)

  ;; Test when the target does NOT equal the given value.
  (check-equal?
    (match-== 42 43 
               (lambda () 'matched)
               (lambda () 'failed))
    'failed)

  ;; Test with string comparison.
  (check-equal?
    (match-== "hello" "hello"
               (lambda () 'ok)
               (lambda () 'not-ok))
    'ok)

  ;; Test with list comparison.
  (check-equal?
    (match-== '(1 2 3) '(1 2 3)
               (lambda () 'yes)
               (lambda () 'no))
    'yes))
 
; ——————————————————-
; Any (Any -> Any) FailureK -> Any
; Matches any value. The success continuation is called
; with the matched target value.
(define (match-var target on-success on-fail)
  (on-success target))

(module+ test
  (require rackunit)

  ;; Test that match-var always succeeds, binding the target value.
  (check-equal?
    (match-var 42 (lambda (v) v) error-on-fail)
    42)

  ;; Another test with a different type (e.g., a list)
  (check-equal?
    (match-var '(a b c)
               (lambda (v) (length v))
               error-on-fail)
    3)

  ;; Test with a string: the success lambda returns a modified version of the string.
  (check-equal?
    (match-var "hello"
               (lambda (s) (string-append s " world"))
               error-on-fail)
    "hello world"))
 
; ——————————————————-
; Any (Any Any -> Any) FailureK -> Any
; Matches a pair value. The success continuation is called
; with the car and cdr of the pair.
 
; Examples:
; (match-cons
;   (cons 1 2)
;   (lambda (car-val cdr-val) (+ car-val cdr-val))
;   error-on-fail)
; Evaluates to:
; 3
; 
; (match-cons
;  4
;  (lambda (car-val cdr-val) (+ car-val cdr-val))
;  error-on-fail)
; Raises the error with message "minimatch: all patterns failed to match"
 
(define (match-cons target on-success on-fail)
  (if (pair? target)
      (on-success (car target) (cdr target))
      (on-fail)))

(module+ test
  (require rackunit)
  ;; Test with an explicit cons cell (not a list):
  (check-equal?
    (match-cons (cons 1 2)
      (lambda (a b) (list a b))
      error-on-fail)
    (list 1 2))

  ;; Test with a two-element list.
  ;; (list 3 4) is (cons 3 (cons 4 '())),
  ;; so we expect a to be 3 and b to be (list 4).
  (check-equal?
    (match-cons (list 3 4)
      (lambda (a b) (list a b))
      error-on-fail)
    (list 3 (list 4)))  ; expected: (3 (4))

  ;; Test with a list of more than two elements.
  (check-equal?
    (match-cons (list 3 4 5)
      (lambda (a b) (list a b))
      error-on-fail)
    (list 3 (list 4 5)))

  ;; Test failure: when target is not a pair.
  (check-equal?
    (match-cons 5
      (lambda (a b) (list a b))
      (lambda () 'failed))
    'failed))

(module+ test
  (require rackunit)
  
  ;; Hand-translation for:
  ;; (match (cons 1 2)
  ;;   [(cons a b) (+ a b)])
  (check-equal?
   (match-cons (cons 1 2)
     (lambda (a-val b-val)
       (match-var a-val
         (lambda (a)
           (match-var b-val
             (lambda (b) (+ a b))
             error-on-fail))
         error-on-fail))
     error-on-fail)
   3)

  ;; Hand-translation for:
  ;; (match (list 3 4 5)
  ;;   [(cons a (cons b (cons c '())))
  ;;    (+ a b c)])
  (check-equal?
   (match-cons (list 3 4 5)
     (lambda (a-val rest1)
       (match-var a-val
         (lambda (a)
           (match-cons rest1
             (lambda (b-val rest2)
               (match-var b-val
                 (lambda (b)
                   (match-cons rest2
                     (lambda (c-val rest3)
                       (match-var c-val
                         (lambda (c) (+ a b c))
                         error-on-fail))
                     error-on-fail))
                 error-on-fail))
             error-on-fail))
         error-on-fail))
     error-on-fail)
   12))

(provide match-cons match-var error-on-fail match-==)
#lang eopl
(require racket)
(require rackunit)

#|
Exercise 2.10 - Add to the environment interface a constructor extend-env*, and
implement it using the a-list representation. This constructor takes a list of variables,
a list of values of the same length, and an environment, and is specified by

(extend-env* (var1 ... varn) (val1 ... valn) F) = G
|#

(define empty-env
  (λ () '()))

(define extend-env
  (λ (var val env)
    (cons (cons var val) env)))

(define report-no-binding-found
  (λ (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define apply-env
  (lambda (env search)
    (cond
      [(null? env) (report-no-binding-found search)]
      [(eqv? (car (car env)) search) (cdr (car env))]
      [else (apply-env (cdr env) search)])))

(define extend-env*
  (λ (var-list val-list env)
    (if (null? var-list)
        env
        (extend-env* (cdr var-list)
                     (cdr val-list)
                     (extend-env (car var-list) (car val-list) env))
      )))

(test-equal? "with single binding" (extend-env* '(x) '(3) (empty-env)) '((x . 3)))
(test-equal? "with two bindings" (extend-env* '(x y) '(3 4) (empty-env)) '((y . 4) (x . 3)))
(test-equal? "with many bindings"
             (extend-env* '(x y z a b c) '(1 4 2 3 5 6) (empty-env))
             '((c . 6) (b . 5) (a . 3) (z . 2) (y . 4) (x . 1)))
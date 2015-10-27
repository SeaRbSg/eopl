#lang eopl
(require racket)
(require rackunit)

#|
Exercise 2.9 Add to the environment interface an observer called has-binding?
that takes an environment env and a variable s and tests to see if s has an associated
value in env. Implement it using the a-list representation.
|#

(define empty-env
  (λ () '()))

(test-equal? "creates empty environment" (empty-env) '())

(define empty-env?
  (λ (env)
    (null? env)))

(test-true "empty env is empty" (empty-env? (empty-env)))
(test-false "env with bound variable is not empty" (empty-env? '((x . 3))))

(define has-binding?
  (λ (env s)
    (cond
      [(null? env) #f]
      [(eqv? (caar env) s) #t]
      [else (has-binding? (cdr env) s)]
    )))

(test-false "empty env does not have variable s" (has-binding? (empty-env) 's))
(test-true "env with single variable binding has s" (has-binding? '((s . 3)) 's))
(test-false "env with single variable binding s does not have t" (has-binding? '((s . 3)) 't))
(test-true "env with multiple variable bindings has binding" (has-binding? '((x . 3) (y . 4)) 'y))

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
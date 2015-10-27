#lang eopl
(require racket)
(require rackunit)

#|
Exercise 2.8 Add to the environment interface an observer called empty-env?
and implement it using the a-list representation
|#

(define empty-env
  (λ () '()))

(test-equal? "creates empty environment" (empty-env) '())

(define empty-env?
  (λ (env)
    (null? env)))

(test-true "empty env is empty" (empty-env? (empty-env)))
(test-false "env with bound variable is not empty" (empty-env? '((x . 3))))

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


#lang eopl
(require racket)
(require rackunit)

(define empty-env
  (λ () '()))

(test-equal? "creates empty environment" (empty-env) '())

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
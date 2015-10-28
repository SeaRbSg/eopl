#lang eopl
(require racket)
(require rackunit)

; 2.9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     already done more extensively in 2.6 - second idea as (fetch

(define has-binding?
  (lambda (var env)
    (cond
      [(null? env) #f]
      [(equal? var (car (car env))) #t]
      [(has-binding? var (cdr env))])))


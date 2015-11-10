#lang eopl
(require racket)
(require rackunit)

#|
Exercise 2.21 Implement the data type of environments, as in section 2.2.2, using
define-datatype. Then include has-binding? of exercise 2.9
|#

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val number?)
   (rest-of-env environment?)))

(define has-binding?
  (λ (env s)
    (cases environment env
      [empty-env () #f]
      [extend-env (var val rest-of-env)
                  (or (eqv? var s)
                      (has-binding? rest-of-env s))])))

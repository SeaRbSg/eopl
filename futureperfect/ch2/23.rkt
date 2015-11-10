#lang eopl
(require racket)
(require rackunit)

#|
Exercise 2.23 The definition of lc-exp ignores the condition in definition 1.1.8
that says "Identifier is any symbol other than lambda." Modify the definition of
identifier to capture this condition. As a hint, remember that any predicate
can be used in define-datatype, even ones you define.
|#

(define my-identifier?
  (λ (v)
    (and (symbol? v)
         (not (eqv? v 'lambda)))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var my-identifier?))
  (lambda-exp
   (bound-var my-identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
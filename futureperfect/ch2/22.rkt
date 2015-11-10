#lang eopl
(require racket)
(require rackunit)

#|
Exercise 2.22 Using define-datatype, implement the stack data type of exercise 2.4
|#

(define-datatype stack stack?
  (empty-stack)
  (push
   (val (or number? symbol?))
   (stk stack?)))

(define empty-stack?
  (λ (S)
    (cases stack S
      (empty-stack () #t)
      (push (val stk) #f))))

(define pop
  (λ (S)
    (cases stack S
      (empty-stack () (eopl:error 'pop "Stack is empty"))
      (push (val stk) stk))))

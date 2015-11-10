#lang eopl
(require racket)
(require rackunit)

#|
Exercise 2.12: Implement the stack data type of exercise 2.4 using a procedural representation
|#

(define empty-stack
  (λ ()
    (λ ()
      (eopl:error 'stack "stack is empty"))))

(define empty-stack?
  (λ (stack)
    (eqv? stack (empty-stack))))

(define push
  (λ (e stack)
    (λ ()
      (list e stack))))

(define pop
  (λ (stack)
    (cadr (stack))))

(define top
  (λ (stack)
    (car (stack))))

(test-true "empty stack is empty" (empty-stack? (empty-stack)))
(test-false "non-empty stack is not empty" (empty-stack? (push 3 (empty-stack))))

(test-exn "raises error popping an empty stack" exn:fail? (lambda () (pop (empty-stack))))

(test-equal? "correctly identifies top of stack" (top (push 3 (empty-stack))) 3)


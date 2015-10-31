#lang racket

(require rackunit)

(define b-zero '())

(define b-is-zero?
  (lambda (n)
    (eq? n b-zero)))

[check-equal? (b-is-zero? '()) #t]

(define b-inc
  (lambda (n)
    (cond
      [(b-is-zero? n) '(1)] ; Zero
      [(eq? (car n) 15) ; Carry
       (if (b-is-zero? (cdr n)) ; Carry off the top?
         '(0 1) ; Add a new digit
         (cons 0 (b-inc (cdr n))))] ; Else just increment the next digit
      [else (cons (+ 1 (car n)) (cdr n))])))

[check-equal? (b-inc '(15)) '(0 1)]        ; Carry?
[check-equal? (b-inc '(0 15 1)) '(1 15 1)] ; Plain 'ol increment
[check-equal? (b-inc '(15 15 1)) '(0 0 2)] ; Carry twice?
[check-equal? (b-inc '(15 15)) '(0 0 1)] ; Carry twice and add a digit?

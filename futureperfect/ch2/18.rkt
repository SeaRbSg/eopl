#lang eopl
(require racket)
(require rackunit)

; Exercise 2.18

(define number->sequence
  (lambda (n)
    (list n '() '())))

(test-equal? "creates a sequence" (number->sequence 7) '(7 () ()))   

(define current-element
  (lambda (sequence)
    (car sequence)))

(test-equal? "identify current element" (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)

(define at-left-end?
  (lambda (sequence)
    (null? (second sequence))))

(test-true "at left end of list" (at-left-end? '(6 () (7 8 9))))
(test-false "not at left end of list" (at-left-end? '(6 (5 4 3 2) (7 8 9))))

(define at-right-end?
  (lambda (sequence)
    (null? (third sequence))))

(test-true "at right end of list" (at-right-end? '(6 (5 4 3 2) ())))
(test-false "not at right end of list" (at-right-end? '(6 (5 4 3) (7 8))))

(define insert-to-left
  (lambda (el sequence)
    (list (first sequence)
          (cons el (second sequence))
          (third sequence))))

(test-equal? "insertion to left" (insert-to-left 13 '(6 (5 4 3) (7 8 9))) '(6 (13 5 4 3) (7 8 9)))
(test-equal? "insertion to left when at left end" (insert-to-left 13 '(6 () (7 8 9))) '(6 (13) (7 8 9)))

(define insert-to-right
  (lambda (el sequence)
    (list (first sequence)
          (second sequence)
          (cons el (third sequence)))))

(test-equal? "insertion to right" (insert-to-right 13 '(6 (5 4 3) (7 8 9))) '(6 (5 4 3) (13 7 8 9)))
(test-equal? "insertion to right when at right end" (insert-to-right 13 '(6 (5 4 3) ())) '(6 (5 4 3) (13)))

(define move-to-left
  (lambda (sequence)
    (if (at-left-end? sequence)
        (eopl:error 'sequence-out-of-bounds "Already at left end")
        (list (first (second sequence))
              (cdr (second sequence))
              (cons (first sequence) (third sequence))))))

(test-equal? "moves to left" (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
(test-exn "raises error moving past left extent" exn:fail? (lambda () (move-to-left '(6 () (7 8 9)))))

(define move-to-right
  (lambda (sequence)
    (if (at-right-end? sequence)
        (eopl:error 'sequence-out-of-bounds "Already at right end")
        (list (first (third sequence))
              (cons (car sequence) (second sequence))
              (cdr (third sequence)))
    )))

(test-equal? "moves to right" (move-to-right '(6 (5 4 3) (7 8 9))) '(7 (6 5 4 3) (8 9)))
(test-exn "raises error moving past right extent" exn:fail? (lambda() (move-to-right '(6 (5 4 3) ()))))


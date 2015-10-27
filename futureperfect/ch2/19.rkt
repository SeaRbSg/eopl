#lang eopl
(require racket)
(require rackunit)

; Exercise 2.19

(define number->bintree
  (lambda (number)
    (list number '() '())))

(test-equal? "creates a tree" (number->bintree 13) '(13 () ()))

(define current-element
  (lambda (tree)
    (car tree)))

(test-equal? "finds current element" (current-element '(13 () ())) 13)

(define at-leaf?
  (lambda (tree)
    (null? tree)))

(test-true "leaf is at leaf" (at-leaf? '()))
(test-false "interior node is not a leaf" (at-leaf? '(13 (number->bintree 12) ())))

(define move-to-left
  (lambda (tree)
    (if (null? tree)
        (eopl:error 'error-out-of-bounds "No left child for ~s" tree)
        (second tree))))

(test-equal? "moves to left" (move-to-left '(13 (12 () ()) (14 () ()))) '(12 () ()))
(test-equal? "moves left to leaf" (move-to-left '(13 () ())) '())
(test-exn "raises error moving left with no left child" exn:fail? (lambda () (move-to-left '())))

(define move-to-right
  (lambda (tree)
    (if (null? tree)
        (eopl:error 'error-out-of-bounds "No right child for ~s" tree)
        (third tree))))

(test-equal? "moves to right" (move-to-right '(13 (12 () ()) (14 () ()))) '(14 () ()))
(test-equal? "moves right to leaf" (move-to-right '(13 () ())) '())
(test-exn "raises error moving right with no right child" exn:fail? (lambda () (move-to-right '())))

(define insert-to-left
  (lambda (el tree)
    (list (first tree)
          (list el (second tree) '())
          (third tree))))

(test-equal? "insert to left" (insert-to-left 15 '(13 (12 () ()) (14 () ())))
                              '(13
                                 (15
                                   (12 () ())
                                   ())
                                 (14 () ())))

(define insert-to-right
  (lambda (el tree)
    (list (first tree)
          (second tree)
          (list el
                '()
                (third tree)))))

(test-equal? "insert to right" (insert-to-right 15 '(13 (12 () ()) (14 () ())))
             '(13
                (12 () ())
                (15
                  ()
                  (14 () ()))))


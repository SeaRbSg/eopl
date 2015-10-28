#lang eopl
(require racket)
(require rackunit)

; 2.18 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NodeInSequence ::= (Int Listof(Int) Listof(Int))

(define number->sequence (λ(N) (list N '() '())))           ; constructor
(define current-element  (λ(NIS) (car NIS)))              ; extractor

; mine - auxiliary
(define head-seq (lambda (l) (if (null? l) (eopl:error 'head "404") (car l))))
(define tail-seq (lambda (l) (if (or (null? l) (null? (cdr l))) (eopl:error 'tail "404") (cdr l))))
(define preceding (λ(NIS) (car (cdr NIS))))               ; extractor
(define following (λ(NIS) (car (cdr (cdr NIS)))))         ; extractor

(define move-to-left
  (λ(NIS)
    (list (head-seq (preceding NIS))
          (cdr (preceding NIS))
          (cons (current-element NIS) (following NIS)))))

(define move-to-right
  (λ(NIS)
    (list (head-seq (following NIS))
          (cons (current-element NIS) (preceding NIS))
          (cdr (following NIS)))))

(define insert-to-left
  (λ(n NIS)
    (list (current-element NIS)
          (cons n (preceding NIS))
          (following NIS))))

(define insert-to-right
  (λ(n NIS)
    (list (current-element NIS)
          (preceding NIS)
          (cons n (following NIS)))))

; this is my interpretation of what left and right end are
(define at-left-end?  (λ(NIS) (null? (preceding NIS))))
(define at-right-end? (λ(NIS) (null? (following NIS))))

(check-equal? (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
(check-equal? (move-to-left '(6 (5) ())) '(5 () (6)))
(check-exn exn:fail? (lambda () (move-to-left '(6 () (7 8 9)))))

(check-equal? (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9)))
(check-equal? (move-to-right '(6 () (7))) '(7 (6) ()))
(check-exn exn:fail? (lambda () (move-to-right '(6 () ()))))

(check-equal? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9)))
(check-equal? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9)))

(check-true  (at-left-end?  '(6 () (7 8 9))))
(check-true  (at-right-end? '(6 (5 4 3 2 1) ())))
(check-false (at-left-end?  '(6 (5) (7 8 9))))
(check-false (at-right-end? '(6 (5 4 3 2 1) (7))))

#lang eopl
(require racket)
(require rackunit)

; 2.19 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bintree ::= () | (Int Bintree Bintree)

; mine
(define fuckup (λ() (eopl:error 'fuckup "404")))
(define left-branch  (λ(t) (car (cdr t))))
(define right-branch (λ(t) (car (cdr (cdr t)))))

(define number->bintree   (λ(n) (list n '() '())))
(define current-node      (λ(t) (if (at-leaf? t) (fuckup) (car t))))
(define move-to-left-son  (λ(t) (if (at-leaf? t) (fuckup) (left-branch t))))
(define move-to-right-son (λ(t) (if (at-leaf? t) (fuckup) (right-branch t))))
(define at-leaf? (λ(t) (null? t)))

(define insert-to-left-branch
  (λ(n t) (list (current-node t)
                (list n (left-branch t) '())      ; i assume we push the tree below to the left
                (right-branch t))))

(define insert-to-right-branch
  (λ(n t) (list (current-node t)
                (left-branch t)
                (list n '() (right-branch t)))))  ; i assume we push the tree below to the right

(define t1 (insert-to-right-branch 14 (insert-to-left-branch 12 (number->bintree 13))))
(check-equal? (number->bintree 13) '(13 () ()))
(check-equal? t1 '(13 (12 () ()) (14 () ())))
(check-equal? (move-to-left-son t1) '(12 () ()))
(check-true   (at-leaf? (move-to-right-son (move-to-left-son t1))))
(check-equal? (insert-to-left-branch 15 t1) '(13 (15 (12 () ()) ()) (14 () ())))

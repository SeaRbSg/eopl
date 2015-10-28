#lang eopl
(require racket)
(require rackunit)

; 2.20 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bintree  ::= () | (Int (Bintree Bintree))
; Memotree ::= () | tree | (Bintree Memotree)   ; stack of Bintrees
; Sototree ::= (Bintree Memotree)

(define fuckup (λ() (eopl:error 'fuckup "404")))
(define left-branch  (λ(t) (car (cdr t))))
(define right-branch (λ(t) (car (cdr (cdr t)))))
(define number->bintree   (λ(n) (list n '() '())))
(define at-leaf? (λ(t) (null? t)))
(define current-node      (λ(t) (if (at-leaf? t) (fuckup) (car t))))
(define move-to-left-son  (λ(t) (if (at-leaf? t) (fuckup) (left-branch t))))
(define move-to-right-son (λ(t) (if (at-leaf? t) (fuckup) (right-branch t))))
(define insert-to-left-branch
  (λ(n t) (list (current-node t)
                (list n (left-branch t) '())      ; i assume we push the tree below to the left
                (right-branch t))))
(define insert-to-right-branch
  (λ(n t) (list (current-node t)
                (left-branch t)
                (list n '() (right-branch t)))))  ; i assume we push the tree below to the right

(define memo->empty (λ() '()))
(define memo->push  (λ(x m) (cons x m)))
(define memo->pull  (λ(m) (cdr m)))

(define st-tree (lambda (st) (car st)))
(define st-memo (lambda (st) (car (cdr st))))

(define n->sototree (lambda (n) (list (number->bintree n) (memo->empty))))
(define st-node (lambda (st) (current-node (st-tree st))))

(define soto->go-left
  (lambda (st)
    (let ([T (st-tree st)] [M (st-memo st)])
      (list (move-to-left-son T)
            (memo->push (list (current-node T) #f (right-branch T)) M)))))

(define soto->go-right
  (lambda (st)
    (let ([T (st-tree st)] [M (st-memo st)])
      (list (move-to-right-son T)
            (memo->push (list (current-node T) (left-branch T) #f) M)))))

(define soto->add-left
  (lambda (n st)
    (list (insert-to-left-branch n (st-tree st)) (st-memo st))))

(define soto->add-right
  (lambda (n st)
    (list (insert-to-right-branch n (st-tree st)) (st-memo st))))

(define soto->go-up
  (lambda (st)
    (let ([dad (car (st-memo st))] [T (st-tree st)])
      (list
       (cons (current-node dad)
             (if (left-branch dad) (list (left-branch dad) T) (list T (right-branch dad))))
       (memo->pull (st-memo st))))))

(define @leaf? (λ(st) (at-leaf? (st-tree st))))
(define @root? (λ(st) (null? (st-memo st))))

(define t1 (insert-to-right-branch 14 (insert-to-left-branch 12 (number->bintree 13))))
(define t3 (soto->add-right 14 (soto->add-left 12 (n->sototree 13))))
(check-equal? t3 (list t1 (memo->empty)))
(check-equal? (soto->go-left t3) '((12 () ()) ((13 #f (14 () ())))))
(check-equal? (soto->go-up (soto->go-left t3)) t3)
(check-true   (@root? (soto->go-up (soto->go-right (soto->go-up (soto->go-left t3))))))

#lang eopl
(require racket)
(require rackunit)

; 2.24 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define bintree-to-list
  (lambda (T)
    (cases bintree T
      (leaf-node (n) (list 'leaf-node n))
      (interior-node (k l r) (list k (bintree-to-list l) (bintree-to-list r))))))

(check-equal? (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
                '(a (leaf-node 3) (leaf-node 4)))

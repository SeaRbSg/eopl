#lang eopl
(require racket)
(require rackunit)

#|
Exercise 2.24 Implement a bintree-to-list procedure for binary trees, so that
(bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4))) returns the list

(interior-node
a
(leaf-node 3)
(leaf-node 4))
|#

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define bintree-to-list
  (λ (tree)
    (cases bintree tree
      (leaf-node (val) (leaf-node val))
      (interior-node (key left right)
                     (interior-node key
                                    (bintree-to-list left)
                                    (bintree-to-list right))))))

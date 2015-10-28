#lang eopl
(require racket)
(require rackunit)

; 2.26 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Red-blue-tree    ::= Red-blue-subtree
; Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;                  ::= (blue-node {Red-blue-subtree}∗)
;                  ::= (leaf-node Int)

(define-datatype tree-rb tree-rb?
  (leafy-node
   (num integer?))
  (red-node
   (rbt1 tree-rb?)(rbt2 tree-rb?))
  (blue-node-single
    (rbt1 tree-rb?))
  (blue-node-multi
    (rbt1 tree-rb?)
    (blue-node-multi tree-rb?)))

(define mark-red-depth
  (lambda (tree)
    (letrec
        ([M (lambda (t n)
              (cases tree-rb t
                (leafy-node (m) (leafy-node n))
                (blue-node-single (head) (blue-node-single (M head n)))
                (blue-node-multi (head tail) (blue-node-multi (M head n) (M tail n)))
                (red-node (t1 t2)
                          (red-node (M t1 (+ n 1)) (M t2 (+ n 1))))))])
      (M tree 0))))

(define pepe
  (red-node
   (blue-node-multi
    (leafy-node 26)
    (leafy-node 12))
   (red-node
    (leafy-node 11)
    (blue-node-multi
     (leafy-node 117)
     (leafy-node 14)))))

(check-equal? (mark-red-depth pepe)
                (red-node
                 (blue-node-multi
                  (leafy-node 1)
                  (leafy-node 1))
                 (red-node
                  (leafy-node 2)
                  (blue-node-multi
                   (leafy-node 2)
                   (leafy-node 2)))))

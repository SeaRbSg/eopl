#lang eopl
(require racket)
(require rackunit)

; 2.25 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2 (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3 (interior-node 'baz tree-2 (leaf-node 1)))

(define tree-key
  (lambda (T)
    (cases bintree T
      (interior-node (k l r) k)
      (else #f))))

(define max-interior ; pretty fugly
  (lambda (T)
    (let ([max-key #f] [max-val 0])
      (letrec ((traverse
                (lambda (t)
                  (cases bintree t
                    (leaf-node (n) n)
                    (interior-node (k l r)
                                   (let ([ML (traverse l)] [MR (traverse r)])
                                     (let ([MX (max (+ ML MR) ML MR)])
                                       (cond
                                         [(equal? ML MX) (set! max-key (tree-key l))]
                                         [(equal? MR MX) (set! max-key (tree-key r))]
                                         [else (set! max-key k)])
                                       (set! max-val MX))
                                     (+ ML MR)))))))
        (traverse T)
        (list max-key max-val)))))

(check-equal? (max-interior tree-2) '(foo 5))
(check-equal? (max-interior tree-3) '(baz 5))

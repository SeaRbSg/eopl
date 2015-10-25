#lang eopl

(require rackunit)

;; Ex 1.4
; List-of-Int
; => (Int . List-of-Int)
; => (Int . (Int . List-of-Int))
; => (Int . (Int . (Int . List-of-Int)))
; => (Int . (Int . (Int . ())))
; => (Int . (Int . (14 . ())))
; => (Int . (3. (14 . ())))
; => (-7 . (3. (14 . ())))

;; Exercise 1.6
; We would miss the () 0 case

;; Exercise 1.7
; nth-element : List x Int -> SchemeVal
; usage: (nth-element lst n) = the n-th element of lst
(define nth-element
  (lambda (lst n)
    (letrec
           [(nth-element-helper
             (lambda (lst1 n1)
               (if (null? lst1)
                 (report-list-too-short lst n)
                 (if (zero? n1)
                   (car lst1)
                   (nth-element-helper (cdr lst1) (- n1 1))))))]
           (nth-element-helper lst n))))

(define report-list-too-short
  (lambda (lst n)
    (eopl:error 'nth-element
                "~a does not have ~s elements.~%" lst n)))

(check-equal? (nth-element '(a b c d e) 3) 'd)
; (nth-element '(a b c) 5)


; remove-first : Sym : Listof(Sym) -> Listof(Sym)
; usage: (remove-first s los) returns a list with the
;        same elements arranged in the same order as los,
;        except that the first occurence of the symbol s
;        is removed.
(define remove-first
  (lambda (s los)
    (if (null? los)
      '()
      (if (eqv? (car los) s)
        (cdr los)
        (cons (car los) (remove-first s (cdr los)))))))

(check-equal? (remove-first 'c '(a b c d)) '(a b d))

;; Exercise 1.8
; this would not return the begining of the list. so it would only return
; the remaining list after the occurance of s.

;; Exercise 1.9
; remove : Sym : Listof(Sym) -> Listof(Sym)
; usage: (remove s los) returns a list with the
;        same elements arranged in the same order as los,
;        except that all occurences of the symbol s are
;        removed.
(define remove
  (lambda (s los)
    (if (null? los)
      '()
      (if (eqv? (car los) s)
        (remove s (cdr los))
        (cons (car los) (remove s (cdr los)))))))

(check-equal? (remove 'c '(a c b c d c)) '(a b d))

;; Exercise 1.10
; I guess exclusive-or? xor?

;; Exercise 1.11

;; Exercise 1.12
; subst : Sym x Sym x S-list -> S-list
(define subst
  (lambda (new old slist)
    (if (null? slist)
      '()
      (cons
        ;; "inlined" subst-in-sexp
        (let [(sexp (car slist))]
          (if (symbol? sexp)
            (if (eqv? sexp old) new sexp)
            (subst new old sexp)))
        (subst new old (cdr slist))))))

(check-equal? (subst 'a 'b '((b c) (b () d)))
              '((a c) (a () d)))

;; Exercise 1.13
; S-list ::= ({S-exp}*)
; S-exp := Symbol | S-list

; subst-map : Sym x Sym x S-list -> S-list
(define subst-map
  (lambda (new old slist)
    (map
      (lambda (sexp)
        (if (symbol? sexp)
          (if (eqv? sexp old) new sexp)
          (subst-map new old sexp)))
      slist)))

(check-equal? (subst-map 'a 'b '((b c) (b () d)))
              '((a c) (a () d)))

;; Exercise 1.14

;; Exercise 1.15
; duple: Int x S-exp -> S-list
; usage: (duple n x) returns a lis containing n copies of x
(define (duple n x)
  (if (zero? n)
    '()
    (cons x (duple (- n 1) x))))

(check-equal? (duple 2 3) '(3 3))
(check-equal? (duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha)))
(check-equal? (duple 0 '(blah)) '())

;; Exercise 1.16
; invert: S-list -> S-list
; usage: (invert lst) where lst is a list of 2-lists (lists of length two),
;        return a list with each 2-list reversed.
(define (invert lst)
  (map
    (lambda (pair)
      (cons (cadr pair)
            (cons (car pair) '())))
  lst))

(check-equal? (invert '((a 1) (a 2) (1 b) (2 b)))
              '((1 a) (2 a) (b 1) (b 2)))

;; Exercise 1.17
; down: S-list -> S-list
; usage: (down lst) wraps parentheses around each top-level element of lst.
(define (down lst)
  (map
    (lambda (sexp)
      (cons sexp '()))
    lst))

(check-equal? (down '(1 2 3))
              '((1) (2) (3)))

(check-equal? (down '((a) (fine) (idea)))
              '(((a)) ((fine)) ((idea))))

(check-equal? (down '(a (more (complicated)) object))
              '((a) ((more (complicated))) (object)))

;; Exercise 1.18
; swrapper: Sym x Sym x S-list -> S-list
; usage: (swrapper s1 s2 slist) returns a list the same as slist, but
;        with all occurances of s1 replaced by s3 and all occurances of s2
;        replaced by s1.
(define (swrapper s1 s2 slist)
  (if (null? slist)
    '()
    (let ([sexp (car slist)])
      (cons
        (if (pair? sexp)
          (swrapper s1 s2 sexp)
          (cond [(eqv? sexp s1) s2]
                [(eqv? sexp s2) s1]
                [else sexp]))
        (swrapper s1 s2 (cdr slist))))))

(check-equal? (swrapper 'a 'd '(a b c d))
              '(d b c a))

(check-equal? (swrapper 'a 'd '(a d () c d))
              '(d a () c a))

(check-equal? (swrapper 'x 'y '((x) y (z (x))))
              '((y) x (z (y))))

;; Exercise 1.19
; list-set: S-list x Int x S-exp -> S-list
; usage: (list-set lst n x) returns a list like lst, except that the n-th
;        element, using zero-based indexing, is x.
(define (list-set lst n x)
  (if (zero? n)
    (cons x (cdr lst))
    (cons (car lst)
          (list-set (cdr lst) (- n 1) x))))

(check-equal? (list-set '(a b c d) 2 '(1 2))
              '(a b (1 2) d))

(check-equal? (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)
              '(1 5 10))

;; Exercise 1.20
; count-occurrences: S-exp x S-List -> Int
; usage: (count-occurrences s slist) returns the number of occurrences of s in slist.
(define (count-occurrences s slist)
  (if (null? slist)
    0
    (let ([sexp (car slist)])
      (+ (if (pair? sexp)
           (count-occurrences s sexp)
           (if (eqv? sexp s) 1 0))
         (count-occurrences s (cdr slist))))))

(check-equal? (count-occurrences 'x '((f x) y (((x z) x)))) 3)
(check-equal? (count-occurrences 'x '((f x) y (((x z) () x)))) 3)
(check-equal? (count-occurrences 'w '((f x) y (((x z) x)))) 0)

;; Exercise 1.21

; product-partial: Sym x List-of(Sym) -> List-of(List-of(Sym))
; usage: (product-partial s sos), where s is a symbol and sos is a list of symbols
;        without reptitions, returns a list of 2-lists that represent the
;        partial Cartesian product of s and sos
(define (product-partial s sos)
  (if (null? sos)
    '()
    (cons (list s (car sos))
          (product-partial s (cdr sos)))))

(check-equal? (product-partial 'a '(x y))
              '((a x) (a y)))

; product: List-of(Sym) x List-of(Sym) -> List-of(List-of(Sym))
; usage: (product sos1 sos2), where sos1 and sos2 are each a list
;        of symbols without repetitions, returns a list of 2-lists that represents the Cartesian
;        product of sos1 and sos2. The 2-lists may appear in any order.
(define (product sos1 sos2)
  (if (null? sos1)
    '()
    (append (product-partial (car sos1) sos2)
          (product (cdr sos1) sos2))))

(check-equal? (product '(a b c) '(x y))
              '((a x) (a y) (b x) (b y) (c x) (c y)))

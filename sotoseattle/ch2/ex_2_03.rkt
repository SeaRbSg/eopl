#lang eopl
(require racket)
(require rackunit)

; 2.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)

; 1. Every number has inifinitely representations because for any number n can be expressed as
; the difference between itself and zero
; (n) => (diff (n) (diff (one) (one))
; where recursively we can substitute (one) at ad-infinitum with the following rule:
; (one) => (diff (one) (diff (one) (one)))

(define diff
  (lambda (a b)
    (cons 'diff (cons a (cons b '())))))

(define one
  (lambda ()
    '(one)))

(define zerod
  (lambda ()
    (diff (one) (one))))

(define is-zerod?
  (lambda (t)
    (cond
      [(equal? (car t) 'diff) (equal? (car (cdr t)) (car (cdr (cdr t))))]
      [else #f])))

(check-true (is-zerod? (diff (one) (one))))

;(define firstD (lambda (t) (car (cdr t))))
;(define seconD (lambda (t) (car (cdr (cdr t)))))
(define firstD
  (lambda (t)
    (cond
      [(equal? t '(one)) '(one)]
      [(car (cdr t))])))

(define seconD
  (lambda (t)
    (cond
      [(equal? t '(one)) (zerod)]
      [(car (cdr (cdr t)))])))

(define to-intd
  (lambda (t)
    (cond
      [(equal? t '(one)) 1]
      [(- (to-intd (firstD t)) (to-intd (seconD t)))])))

(check-equal? (to-intd (diff (one) (one))) 0)
(check-equal? (to-intd (diff (one) (diff (one) (one)))) 1)
(check-equal? (to-intd (diff (diff (one) (one)) (diff (one) (zerod)))) -1)
(check-equal? (to-intd (diff (diff (one) (diff (one) (one)))
                                (diff (diff (one) (one)) (diff (one) (zerod))))) 2)

(define one+
  (lambda ()
    (diff
     (diff (one) (zerod))
     (diff (zerod) (diff (one) (zerod))))))

(define successord
  (lambda (t)
    (cond
      [(equal? t '(one)) (one+)]
      [(diff (successord (firstD t)) (seconD t))])))

(check-equal? (to-intd (successord (zerod))) 1)
(check-equal? (to-intd (successord (diff (one) (one)))) 1)
(check-equal? (to-intd (successord (successord (diff (one) (one))))) 2)
(check-equal? (to-intd (successord (successord (successord (diff (one) (one)))))) 3)

(define predecessord
  (lambda (t)
    (cond
      [(equal? t '(one)) (one+)]
      [(diff (firstD t) (predecessord (seconD t)))])))

(check-equal? (to-intd (predecessord (zerod))) -1)
(check-equal? (to-intd (predecessord (diff (one) (one)))) -1)
(check-equal? (to-intd (predecessord (predecessord (diff (one) (one))))) -2)
(check-equal? (to-intd (predecessord (predecessord (predecessord (diff (one) (one)))))) -3)

(check-equal? (to-intd (successord (successord (successord
                           (predecessord (predecessord (predecessord (zerod)))))))) 0)

(define nono
  (lambda (t)
    (if (eqv? t '(one))
        t
        (diff (seconD t) (firstD t)))))

(define diff-tree-plus ; needs refactor and more thought
  (lambda (b1 b2)
    (diff b1 (nono b2))))

(check-equal? (to-intd (diff-tree-plus (zerod) (zerod))) 0)
(check-equal? (to-intd (diff-tree-plus (one) (one))) 2)
(check-equal? (to-intd (diff-tree-plus (one)
                                 (diff (diff (one) (diff (one) (one)))
                                       (diff (diff (one) (one)) (diff (one) (zerod)))))) 3)
(check-equal? (to-intd (diff-tree-plus (diff (diff (one) (diff (one) (one)))
                                       (diff (diff (one) (one)) (diff (one) (zerod))))
                                 (one))) 3)
(check-equal? (to-intd (diff-tree-plus (zerod) (diff (one) (diff (one) (one))))) 1)
(check-equal? (to-intd (diff-tree-plus (diff (one) (diff (one) (one))) (zerod))) 1)
(check-equal? (to-intd (diff-tree-plus (diff (one) (diff (one) (one)))
                                 (diff (one) (diff (one) (one))))) 2)
(check-equal? (to-intd (diff-tree-plus
                           (diff (diff (one) (diff (one) (one)))
                                 (diff (diff (one) (one)) (diff (one) (zerod))))                 ; 2
                           (predecessord (predecessord (predecessord (diff (one) (one))))))) -1) ; -3


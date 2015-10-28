#lang eopl
(require racket)
(require rackunit)

; 2.31 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STUCK!!
; Prefix-list ::=(Prefix-exp)
; Prefix-exp  ::= Int
;             ::= Prefix-exp Prefix-exp

(define-datatype prefix-list prefix-list?
  (list-of-prefixes
   (head-prefix prefix-exp?)
   (rest-prefix prefix-list?)))

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define conchita
  (diff-exp
   (diff-exp
    (const-exp 3)
    (const-exp 2))
   (diff-exp
    (const-exp 4)
    (diff-exp
     (const-exp 12)
     (const-exp 7)))))

;(define prex-parse
;  (lambda (l) l
;    (cond
;      [(null? l) l]
;      [(number? l) (const-exp l)]
;      [(list? l)
;       (cond
;         [(number? (car l)) (prex-parse (car l))]
;         [(eqv? '- (car l))
;         ...
;    ))

;(check-equal? (prex-parse '(2)) (const-exp 2))
;(check-equal? (prex-parse '(- 3 2)) (diff-exp (const-exp 3) (const-exp 2)))
;(prex-parse '(- 3 - 2 4))
;(prex-parse '(- - 3 2 4))
;(prex-parse '(- - 3 2 - 4 - 12 7))
;conchita






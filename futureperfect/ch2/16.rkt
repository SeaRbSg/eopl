#lang eopl
(require racket)
(require rackunit)

#|
Exercise 2.16 Modify the implementation to use a representation in which there
are not parentheses around the bound variable in a lambda expression
|#

; Constructors

; Var -> Lc-exp
(define var-exp
  (λ (var)
    var))

; Var x Lc-exp -> Lx-exp
(define lambda-exp
  (λ (var lc-exp)
    `(λ ,var
       ,lc-exp)))

; Lc-exp x Lc-exp -> Lc-exp
(define app-exp
  (λ (exp1 exp2)
    `(,exp1 ,exp2)))

;
; Predicates
;

; Lc-exp -> Bool
(define var-exp?
  (λ (exp)
    (symbol? exp)))

; Lc-exp -> Bool
(define lambda-exp?
  (λ (exp)
    (and (list? exp)
         (eqv? (car exp) 'lambda))))

; Lc-exp -> Bool
(define app-exp?
  (λ (exp)
    (and (list? exp)
         (not (eqv? (car exp) 'lambda)))))

;
; Extractors
;

; Lc-exp -> Var
(define var-exp->var
  (λ (exp)
    exp))

; Lc-exp -> Var
(define lambda-exp->bound-var
  (λ (exp)
    (cadr exp)))

; Lc-exp -> Lc-exp
(define lambda-exp->body
  (λ (exp)
    (caddr exp)))

; Lc-exp -> Lc-exp
(define app-exp->rator
  (λ (exp)
    (car exp)))

; Lc-exp -> Lc-exp
(define app-exp->rand
  (λ (exp)
    (cadr exp)))

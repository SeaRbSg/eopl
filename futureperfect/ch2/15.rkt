#lang eopl
(require racket)
(require rackunit)

#|
Exercise 2.15 Implement the lambda-calculus expression interface for the
representation specified by the grammar above.
|#

; Constructors

; Var -> Lc-exp
(define var-exp
  (λ (var)
    var))

; Var x Lc-exp -> Lx-exp
(define lambda-exp
  (λ (var lc-exp)
    `(λ (,var)
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
    (caadr exp)))

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

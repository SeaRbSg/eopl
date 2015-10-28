#lang eopl
(require racket)
(require rackunit)

; 2.15 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define occurs-free-v0?
  (lambda (V E)
    (cond
      [(symbol? E) (eqv? V E)]
      [(eqv? (car E) 'lambda)
       (and (not (eqv? (caadr E) V))
            (occurs-free-v0? V (caddr E)))]
      [else (or
             (occurs-free-v0? V (car E))
             (occurs-free-v0? V (cadr E)))])))

; Lc-exp ::= Identifier
;        ::= (lambda (Identifier) Lc-exp)
;        ::= (Lc-exp Lc-exp)

; predicates directly from grammar
(define var-exp?     (lambda (V) (symbol? V)))
(define lambda-exp?  (lambda (E) (eqv? (car E) 'lambda)))
(define app-exp?     (lambda (E) (and (pair? E) (procedure? (car E)))))

; constructors
(define var-exp    (lambda (E) E))
(define lambda-exp (lambda (V E) (list 'lambda (V) E)))
(define app-exp    (lambda (E D) (list E D)))

; extractors
(define var-exp->var (lambda (V) V))
(define lambda-exp->bound-var (lambda (E) (car (car (cdr E)))))
(define lambda-exp->body      (lambda (E) (car (cdr (cdr E)))))
(define app-exp->rator        (lambda (E) (car E)))
(define app-exp->rand         (lambda (E) (car (cdr E))))

(define occurs-free?
  (lambda (V E)
    (cond
      ((var-exp? E) (eqv? V (var-exp->var E)))
      ((lambda-exp? E)
       (and
        (not (eqv? V (lambda-exp->bound-var E)))
        (occurs-free? V (lambda-exp->body E))))
      (else (or
             (occurs-free? V (app-exp->rator E))
             (occurs-free? V (app-exp->rand E)))))))


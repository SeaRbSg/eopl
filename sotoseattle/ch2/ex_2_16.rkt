#lang eopl
(require racket)
(require rackunit)

; 2.16 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Lc-exp ::= (lambda Identifier Lc-exp)

; the only ones to change would be:
(define lambda-exp# (lambda (V E) (list 'lambda V E)))     ; constructor
(define lambda-exp->bound-var# (lambda (E) (car (cdr E)))) ; bound var extractor

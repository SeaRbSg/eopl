#lang eopl
(require racket)
(require rackunit)

; 2.10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define report-not-found (lambda () (eopl:error 'apply-env "No binding found")))

(define empty-env-al
  (lambda ()
    '()))

(define empty-env-al?
  (lambda (e)
    (null? e)))

(define extend-env-al
  (lambda (var val env)
    (cons (list var val) env)))

(define apply-env-al
  (lambda (env var)
    (cond
      [(empty-env-al? env) (report-not-found)]
      [(equal? (car (car env)) var) (car (cdr (car env)))]
      [(apply-env-al (cdr env) var)])))

(define extend-env-al*
  (lambda (lvar lval env)
    (cond
      [(null? lvar) env]
      [(extend-env-al (car lvar) (car lval) (extend-env-al* (cdr lvar) (cdr lval) env))])))

(define e10
  (extend-env-al* '(a b c) '(1 2 3)
    (extend-env-al 'm 5
      (extend-env-al* '(x y) '(8 9)
        (empty-env-al)))))

(check-equal? e10 '((a 1) (b 2) (c 3) (m 5) (x 8) (y 9)))

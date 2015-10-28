#lang eopl
(require racket)
(require rackunit)

; 2.4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-env
  (lambda ()
    '(empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

(define report-not-found (lambda () (eopl:error 'apply-env "No binding found")))
(define report-invalid-env (lambda () (eopl:error 'apply-env "Bad Environs")))

(define apply-env
  (lambda (env var)
    (cond
      [(eqv? (car env) '(empty-env)) (report-not-found)]
      [(eqv? (car env) 'extend-env)
       (let ([saved-var (car (cdr env))]
             [saved-val (car (cdr (cdr env)))]
             [saved-env (car (cdr (cdr (cdr env))))])
         (cond
           [(eqv? var saved-var) saved-val]
           [(apply-env saved-env var)]))]
      [else (report-invalid-env)])))

(define e
  (extend-env 'd 6
    (extend-env 'y 8
      (extend-env 'x 7
        (extend-env 'y 14
          (empty-env))))))

(check-equal? (apply-env e 'x) 7)
(check-equal? (apply-env e 'y) 8)
(check-exn exn:fail? (lambda () (apply-env e 'z)))

; I am not sure they are asking for this...

(define empty-stack     ; constructor
  (lambda ()
    '()))

(define push            ; constructor
  (lambda (val stk)
    (cons val stk)))

(define report-end-of-stack  (lambda () (eopl:error 'apply-env "Enf Of Stack")))
(define report-invalid-stack (lambda () (eopl:error 'apply-env "Bad Stack")))

(define empty-stack?    ; observer
  (lambda (stk)
    (null? stk)))

(define top             ; observer
  (lambda (stk)
    (if (empty-stack? stk)
        (report-end-of-stack)
        (car stk))))

(define pop            ; constructor
  (lambda (stk)
    (if (empty-stack? stk)
        (report-end-of-stack)
        (cdr stk))))

(define s
  (push 6
    (push 8
      (push 7
        (push 14
          (empty-stack))))))

(check-false  (empty-stack? s))
(check-equal? (top s) 6)
(check-equal? (top (pop s)) 8)
(check-true   (empty-stack? (pop (pop (pop (pop s))))))


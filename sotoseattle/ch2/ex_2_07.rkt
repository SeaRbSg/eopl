#lang eopl
(require racket)
(require rackunit)

; 2.7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define report-not-found-better
  (lambda (var env)
    (eopl:error 'apply-env "End of Line, ~s nowhere to be found in ~s" var env)))

(define report-invalid-env-better
  (lambda (instr env)
    (eopl:error 'apply-env "WTF? Dunno what ~s is in the environment ~s" instr env)))

(define apply-env-better
  (lambda (env var)
    (cond
      [(eqv? (car env) '(empty-env)) (report-not-found-better var env)]
      [(eqv? (car env) 'extend-env)
       (let ([saved-var (car (cdr env))]
             [saved-val (car (cdr (cdr env)))]
             [saved-env (car (cdr (cdr (cdr env))))])
         (cond
           [(eqv? var saved-var) saved-val]
           [(apply-env-better saved-env var)]))]
      [else (report-invalid-env-better (car env) env)])))

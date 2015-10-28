#lang eopl
(require racket)
(require rackunit)

; 2.21 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Env = (empty-env) | (extend-env Var SchemeVal Env)
; Var = Sym

(define-datatype Env Env?
   (empty-env-dt)        ; where do I place the predicate empty-env-dt?
   (extend-env-dt
    (saved-var symbol?)
    (saved-val number?)
    (saved-env Env?)))

(define apply-env-dt
  (lambda (E V)
    (cases Env E
      (empty-env-dt () #f) ; for me is better with false
      (extend-env-dt (saved-var saved-val saved-env)
                     (if (eqv? saved-var V) saved-val (apply-env-dt saved-env V))))))

(define empty-env-dt?
  (lambda (E)
    (cases Env E
      (empty-env-dt () #t)
      (else #f))))

(define has-binding-dt?
  (lambda (E V)
    (number? (apply-env-dt E V))))

(define edt
  (extend-env-dt 'd 6
    (extend-env-dt 'y 8
      (extend-env-dt 'x 7
        (extend-env-dt 'y 14
          (empty-env-dt))))))

(check-true   (empty-env-dt? (empty-env-dt)))
(check-false  (empty-env-dt? edt))
(check-equal? (apply-env-dt edt 'x) 7)
(check-equal? (apply-env-dt edt 'y) 8)
(check-false  (apply-env-dt edt 'z))
(check-false  (has-binding-dt? edt 'z))
(check-true   (has-binding-dt? edt 'x))
(check-false  (has-binding-dt? (empty-env-dt) 'x))

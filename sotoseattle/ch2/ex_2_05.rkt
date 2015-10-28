#lang eopl
(require racket)
(require rackunit)

; 2.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define eal
  (extend-env-al 'd 6
    (extend-env-al 'y 8
      (extend-env-al 'x 7
        (extend-env-al 'y 14
          (empty-env-al))))))

(check-equal? (apply-env-al eal 'y) 8)
(check-exn exn:fail? (lambda () (apply-env-al eal 'z)))



#lang eopl
(require racket)
(require rackunit)

; 2.6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     the idea is to play between different data structures and nature of (var/val) to store

; First Idea: Use numbers to store numbers

(define empty-env-1 (lambda () 1))
(define empty-env-1? (lambda (e) (equal? e 1)))

(define extend-env-1
  (lambda (n env)
    (* env n)))

(define apply-env-1
  (lambda (env n)
    (and (not (empty-env-1? env)) (equal? (modulo env n) 0))))

(define e1
  (extend-env-1 2
    (extend-env-1 3
      (extend-env-1 5
        (extend-env-1 13
          (empty-env-1))))))

(check-true  (apply-env-1 e1 2))
(check-false (apply-env-1 e1 7))

; Second Idea: List with recursive reference
;   todo: fail because of circularity not enforced !!
;   todo: the order of extensions matter !!

(define report-not-found (lambda () (eopl:error 'apply-env "No binding found")))

(define empty-env-2 (lambda () '()))
(define empty-env-2? (lambda (e) (null? e)))

(define fetch
  (lambda (var env)
    (cond
      [(null? env) env]
      [(equal? var (car (car env)))
       (let ([vl (car (cdr (car env)))])
         (if (symbol? vl)
             (fetch vl env)
             vl))]
      [(fetch var (cdr env))])))

(check-equal? (fetch 'a '((a 1) (x c) (c 3))) 1)
(check-equal? (fetch 'x '((a 1) (x c) (c 3))) 3)
(check-equal? (fetch 'b '((a 1) (x c) (c 3))) '())

(define extend-env-2
  (lambda (var val env)
    (if (null? (fetch var env))
        (cons (list var val) env)
        env)))

(define apply-env-2
  (lambda (env var)
    (let ([eol (fetch var env)])
      (if (null? eol)
          (report-not-found)
          eol))))

(define e2
  (extend-env-2 'd 10
    (extend-env-2 'w 'y
      (extend-env-2 'y 'x
        (extend-env-2 'x 'z
          (extend-env-2 'z 9
            (extend-env-2 'z 1
              (empty-env-2))))))))

(check-equal? (apply-env-2 e2 'z) 1)
(check-equal? (apply-env-2 e2 'x) 1)
(check-equal? (apply-env-2 e2 'w) 1)
(check-exn exn:fail? (lambda () (apply-env-2 e2 'm)))

; Third Idea: Trees // Vectors // Lambda Calculus



#lang eopl
(require rackunit)

; 2.1
(define b-zero '())

(define b-is-zero?
  (λ (n)
    (eq? n b-zero)))

[check-equal? (b-is-zero? '()) #t]

(define b-inc
  (λ (n)
    (cond
      [(b-is-zero? n) '(1)] ; Zero
      [(eq? (car n) 15) ; Carry
       (if (b-is-zero? (cdr n)) ; Carry off the top?
         '(0 1) ; Add a new digit
         (cons 0 (b-inc (cdr n))))] ; Else just increment the next digit
      [else (cons (+ 1 (car n)) (cdr n))])))

[check-equal? (b-inc '(15)) '(0 1)]        ; Carry?
[check-equal? (b-inc '(0 15 1)) '(1 15 1)] ; Plain 'ol increment
[check-equal? (b-inc '(15 15 1)) '(0 0 2)] ; Carry twice?
[check-equal? (b-inc '(15 15)) '(0 0 1)] ; Carry twice and add a digit?


; 2.5
; no

; 2.6
; no

; 2.7
; no

(define empty-env
  (λ () (list 'empty-env)))

; 2.8
(define empty-env?
  (λ (env) 
    (equal? env (list 'empty-env))))

[check-equal? (empty-env? (empty-env)) #t]

('extend-env 'a 'b '(empty-env))

; 2.9
(define has-binding?
  (λ (env s)
    (cond
      [(eqv? (car env) 'empty-env) #f]
      [(eqv? (car env) 'extend-env)
       (let [(saved-var (cadr env))
             (rest-of-env (cadddr env))]
         (if (eqv? s saved-var)
           #t
           (has-binding? rest-of-env s)))]
      [else
        (report-invalid-env env)])))

; 2.10
(define extend-env*
  (λ (vars vals env) 
    (cond
      [(null? vars) env]
      [(null? cdr vars) (list 'extend-env (car vars) (car vals) env)]
      [else 
        (extend-env* (cdr vars) (cdr vals) 
                     (list 'extend-env (car vars) (car vals) env))])))

(define extend-env
  (λ (var val env) 
    (list 'extend-env-env var val env)))

(define apply-env
  (λ (env search-var)
    (cond
      [(eqv? (car env) 'empty-env)
       (report-no-binding-found search-var)]
      [(eqv? (car env) 'extend-env)
       (let ([saved-var (cadr env)]
             [saved-val (caddr env)]
             [saved-env (cadddr env)])
         (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))]
       [else
         (report-invalid-env env)])))

(define report-no-binding-found
  (λ (search-var)
    (eopl:error 'apply-env "No binding found for ~s" search-var)))

(define report-invalid-env
  (λ (env)
    (eopl:error 'apply-env "Bad environment" env)))

(define id (λ (x) x))

; Just going to stub out all of these
(define var-exp? id)
(define var-exp->var id)
(define lambda-exp id)
(define lambda-exp->bound-var id)
(define lambda-exp->body id)
(define identifier? id)
(define app-exp->rator id)
(define app-exp->rand id)

(define occurs-free?
  (λ (search-var expr)
    (cond
      [(var-exp? expr) (eqv? search-var (var-exp->var expr))]; if it's a var, #t
      [(lambda-exp? expr)
       (and
         (not (eqv? search-var (lambda-exp->bound-var expr))); if it's not bound
         (occurs-free? search-var (lambda-exp->body exp)))]; and is in a body, #t
      [else
        (or
          (occurs-free? search-var (app-exp->rator expr))
          (occurs-free? search-var (app-exp->rand expr)))])))

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))


(define occurs-free-with-datatype?
  (λ (search-var expr)
    (cases lc-exp expr
      [var-exp (var) (eqv? var search-var)]
      [lambda-exp (bound-var body)
        (and
          (not (eqv? search-var bound-var))
          (occurs-free-with-datatype? search-var body))]
      [app-exp (rator rand)
        (or
          (occurs-free-with-datatype? search-var rator)
          (occurs-free-with-datatype? search-var rand))])))

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
    (first s-exp?)
    (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp
    (sym symbol?))
  (s-list-s-exp
    (slst s-list?)))

(define parse-expression 
  (λ (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(pair? datum)
       (if (eqv? (car datum) 'λ)
         (lambda-exp
           (car (cadr datum))
           (parse-expression (caddr datum)))
         (app-exp
           (parse-expression (car datum))
           (parse-expression (cadr datum))))]
      [else (report-invalid-concrete-syntax datum)])))

(define report-invalid-concrete-syntax
  (λ (datum)
    (eopl:error "Invalid concrete syntax: ~s" datum)))


; 2.12
(define empty-stackp
    'EMPTY)

(define pushp ; -> stack
  (λ (var stack)
    (λ ()
      (list var stack))))

(define popp ; -> (stack var)
  (λ (stack)
    (stack)))

[check-equal? (popp (pushp 'x empty-stackp)) '(x EMPTY)]

; 2.15
; 2.16
; 2.17
; 2.18
; 2.19
; 2.21
; 2.22
; 2.23
; 2.24
; 2.27
; 2.28
; 2.29

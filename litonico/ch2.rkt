#lang eopl
(require rackunit)

(define b-zero '())

(define b-is-zero?
  (lambda (n)
    (eq? n b-zero)))

[check-equal? (b-is-zero? '()) #t]

(define b-inc
  (lambda (n)
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

; (define b-dec '())

(define empty-env
  (lambda () (list 'empty-env)))

; 2.8
(define empty-env?
  (lambda (env) 
    (equal? env (list 'empty-env))))

[check-equal? (empty-env? (empty-env)) #t]

(define extend-env
  (lambda (var val env) 
    (list 'empty-env var val env)))

(define apply-env
  (lambda (env search-var)
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
  (lambda (search-var)
    (eopl:error 'apply-env "No binding found for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment" env)))

(define id (lambda (x) x))

; Just going to stub out all of these
(define var-exp? id)
(define var-exp->var id)
(define lambda-exp? id)
(define lambda-exp->bound-var id)
(define lambda-exp->body id)
(define identifier? id)
(define app-exp->rator id)
(define app-exp->rand id)

(define occurs-free?
  (lambda (search-var expr)
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
  (lambda (search-var expr)
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
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(pair? datum)
       (if (eqv? (car datum) 'lambda)
         (lambda-exp
           (car (cadr datum))
           (parse-expression (caddr datum)))
         (app-exp
           (parse-expression (car datum))
           (parse-expression (cadr datum))))]
      [else (report-invalid-concrete-syntax datum)])))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error "Invalid concrete syntax: ~s" datum)))


(define empty-stackp
    'EMPTY)

(define pushp ; -> stack
  (lambda (var stack)
    (lambda ()
      (list var stack))))

(define popp ; -> (stack var)
  (lambda (stack)
    (stack)))

[check-equal? (popp (pushp 'x empty-stackp)) '(x EMPTY)]

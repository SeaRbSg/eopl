#lang eopl

(require rackunit)

;; Exercise 2.1
(define N 16) ;; Assume base 16

(define (zero) '())
(define (is-zero? n) (null? n))
(define (successor n)
  (if (null? n)
    '(1)
    (let ([m (+ (car n) 1)])
      (if (eqv? m N)
        (cons 1 (cons 0 (cdr n)))
        (cons m (cdr n))))))
(define (predecessor n)
  (let ([m (- (car n) 1)])
    (if (zero? m)
      (if (null? (cdr n))
        '()
        (cons (- N 1) (cddr n)))
      (cons m (cdr n)))))

(define (plus x y)
  (if (is-zero? x)
    y
    (successor (plus (predecessor x) y))))

(define (mult x y)
  (if (is-zero? x)
    x
    (let ([m (predecessor x)])
      (if (is-zero? m)
        y
        (plus y (mult m y))))))

(define (factorial n)
  (let ([m (predecessor n)])
    (if (is-zero? m)
      n
      (mult n (factorial m)))))

(check-true (is-zero? (zero)))
(check-false (is-zero? '(1)))
(check-equal? (successor (zero)) '(1))
(check-equal? (successor '(1)) '(2))
(check-equal? (successor '(15)) '(1 0))
(check-equal? (predecessor '(2)) '(1))
(check-equal? (predecessor '(1 0)) '(15))
(check-equal? (plus '(1) '(2)) '(3))
(check-equal? (mult '(2) '(3)) '(6))
(check-equal? (mult '(12) '(2)) '(9 0))
(check-equal? (factorial '(3)) '(6))
;; (factorial '(10))

;; Exercise 2.5
(define (report-no-binding-found search-var)
  (eopl:error 'apply-env "No binding found for ~s" search-var))

(define (report-invalid-env env)
  (eopl:error 'apply-env "Bad Environment: ~s" env))

; empty-env : '() -> Env
(define (empty-env) '())

; empty-env : Var x SchemeVal x Env -> Env
(define (extend-env var val env)
  (cons `((,var) (,val)) env))

; apply-env : Env x Var -> SchemeVal
(define (apply-env env search-var)
  (if (null? env)
    (report-no-binding-found search-var)
    (let ([result (search-env env search-var)])
      (if (null? result)
        (apply-env (cdr env) search-var)
        result))))

; search-env : Env x Var -> SchemeVal
(define (search-env env search-var)
  (letrec ([search-env-helper
             (lambda (vars vals)
               (cond [(null? vars) '()]
                     [(null? vals) (report-invalid-env env)]
                     [(eqv? search-var (car vars)) (car vals)]
                     [else (search-env-helper (cdr vars) (cdr vals))]))])
    (search-env-helper (caar env) (cadar env))))

(check-equal? (empty-env) '())
(check-equal? (extend-env 'x 10 (empty-env)) '(((x) (10))))
(check-equal? (extend-env 'y 8 (extend-env 'x 10 (empty-env))) '(((y) (8)) ((x) (10))))
(check-equal? (apply-env (extend-env 'x 10(empty-env)) 'x) 10)
(check-equal? (apply-env (extend-env 'y 8 (extend-env 'x 10 (empty-env))) 'y) 8)

(define e
  (extend-env 'd 6
    (extend-env 'y 8
      (extend-env 'x 7
        (extend-env 'y 14
          (empty-env))))))

(check-equal? (apply-env e 'd) 6)
(check-equal? (apply-env e 'x) 7)
(check-equal? (apply-env e 'y) 8)

;; Exercise 2.6
;; Could use vectors, key-value tuples, or BSTs. But not going to code them.

;; Exercise 2.7
; (apply-env e 'q)
;; Not sure what would make this error message better?

;; Exercise 2.8
(define (empty-env? env)
  (eqv? env (empty-env)))

(check-true (empty-env? (empty-env)))
(check-false (empty-env? (extend-env 'x 10 (empty-env))))

;; Exercise 2.9
; has-binding? : Env x Var -> Bool
(define (has-binding? env search-var)
  (if (null? env)
    #f
    (let ([result (search-env env search-var)])
      (if (null? result)
        (has-binding? (cdr env) search-var)
        #t))))

(check-true (has-binding? e 'd))
(check-true (has-binding? e 'x))
(check-true (has-binding? e 'y))
(check-false (has-binding? e 'q))

;; Exercise 2.10
; extend-env* : Env x List(Var) x List(SchemeVal) -> Env
(define (extend-env* vars vals env)
  (cons `(,vars ,vals) env))

(check-equal? (extend-env* '(x y) '(2 10) (empty-env)) '(((x y) (2 10))))

(define e*
  (extend-env* '(x y) '(7 5)
    (extend-env* '(a b y) '(1 2 3)
      (empty-env))))

(check-equal? (apply-env e* 'x) 7)
(check-equal? (apply-env e* 'y) 5)
(check-equal? (apply-env e* 'b) 2)

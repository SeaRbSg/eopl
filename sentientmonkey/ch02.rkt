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

;; Exercise 2.4

; empty-stack : () -> Stack
(define (empty-stack) '())
; push : Element x Stack -> Stack
(define (push e s) (cons e s))
; pop : Stack -> Stack
(define (pop s) (cdr s))
; top : Stack -> Element
(define (top s) (car s))
; empty-stack? : Stack -> Boolean
(define (empty-stack? s) (null? s))

(define s
  (push 'x
    (push 'y
      (push 'z
        (empty-stack)))))

(check-false (empty-stack? s))
(check-equal? (top s) 'x)
(check-equal? (top (pop (pop s))) 'z)

;; empty-stack, push, & pop are constructors
;; empty-stack? & top are observers

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

;; Exercise 2.12
(define (empty-stack*)
  (lambda ()
    '()))
(define (push* e s)
  (lambda ()
    `(,e ,s)))
(define (pop* s) (cadr (s)))
(define (top* s) (car (s)))
(define (empty-stack?* s) (null? (s)))

(define s*
  (push* 'x
    (push* 'y
      (push* 'z
        (empty-stack*)))))

(check-false (empty-stack?* s*))
(check-equal? (top* s*) 'x)
(check-equal? (top* (pop* s*)) 'y)
(check-equal? (top* (pop* (pop* s*))) 'z)

;; Exercise 2.15
; Lambda calculus grammar
; Le-exp ::= Identifer
;        ::= (lambda (Identifier) Le-exp)
;        ::= (Le-exp Le-exp)


; lambda-exp : Identifier x Le-exp -> Le-exp
(define (lambda-exp var val)
  `(lambda ,var ,val))

; lambda-exp? : Le-exp -> Boolean
(define (lambda-exp? exp)
  (eqv? (car exp) 'lambda))

; lambda-exp->bound-var : Le-exp -> Var
(define (lambda-exp->bound-var exp)
  (cadr exp))

; lambda-exp->body : Le-exp -> Le-exp
(define (lambda-exp->body exp)
  (caddr exp))

(define l (lambda-exp 'x 10))

(check-true (lambda-exp? l))
(check-equal? (lambda-exp->bound-var l) 'x)
(check-equal? (lambda-exp->body l) 10)

;; Exercise 2.16
; I think I already did that? Or I'm missing this completely.

;; Exercise 2.17

; var-exp : Var -> Le-exp
(define (var-exp var)
  `(var ,var))

; var-exp? : Le-exp -> Boolean
(define (var-exp? exp)
  (eqv? (car exp) 'var))

; var-exp->var : Le-exp -> Var
(define (var-exp->var exp)
  (cadr exp))

(define v (var-exp 10))
(check-true (var-exp? v))
(check-equal? (var-exp->var v) 10)

;; only doing one...

;; Exercise 2.18
; NodeInSequence ::= (Int Listof(Int) Listof(Int))

; number->sequence : Int -> NodeInSequence
(define (number->sequence n)
  `(,n () ()))

(check-equal? (number->sequence 7) '(7 () ()))

; at-end-left? : NodeInSequence -> Bool
(define (at-end-left? seq)
  (null? (cadr seq)))

(check-true (at-end-left? '(1 () (2 3))))

; at-end-right? : NodeInSequence -> Bool
(define (at-end-right? seq)
  (null? (caddr seq)))

(check-true (at-end-right? '(1 (2 3) ())))

; current-element : NodeInSequence -> Int
(define (current-element seq)
  (car seq))

(check-equal? (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)

; move-to-left : NodeInSequence -> NodeInSequence
(define (move-to-left seq)
  (cons (caadr seq)
        (list (cdadr seq)
              (cons (car seq) (caddr seq)))))

(check-equal? (move-to-left '(6 (5 4 3 2 1) (7 8 9)))
              '(5 (4 3 2 1) (6 7 8 9)))


; move-to-right : NodeInSequence -> NodeInSequence
(define (move-to-right seq)
  (cons (caaddr seq)
        (list (cons (car seq) (cadr seq))
              (cdaddr seq))))

(check-equal? (move-to-right '(6 (5 4 3 2 1) (7 8 9)))
              '(7 (6 5 4 3 2 1) (8 9)))

; insert-to-left : Int x NodeInSequence -> NodeInSequence
(define (insert-to-left n seq)
  (cons (car seq)
        (list (cons n (cadr seq))
              (caddr seq))))

(check-equal? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
              '(6 (13 5 4 3 2 1) (7 8 9)))

; insert-to-right : Int x NodeInSequence -> NodeInSequence
(define (insert-to-right n seq)
  (cons (car seq)
        (list (cadr seq)
              (cons n (caddr seq)))))

(check-equal? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))
              '(6 (5 4 3 2 1) (13 7 8 9)))

;; didn't add error checking...
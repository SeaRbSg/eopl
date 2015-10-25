#lang eopl
(require racket)
(require rackunit)

; 2.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define N 16)

(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))

(define to-int
  (lambda (biggie)
    (letrec ([B (lambda (b n)
                  (if (null? b) 0
                      (+ (* (car b) (expt N n)) (B (cdr b) (+ n 1)))))])
      (B biggie 0))))

(test-equal? "" (to-int '()) 0)
(test-equal? "" (to-int '(0)) 0)
(test-equal? "" (to-int '(1)) 1)
(test-equal? "" (to-int '(1 2)) 33)
(test-equal? "" (to-int '(2 0 1)) 258)
(test-equal? "" (to-int '(18)) (to-int '(2 1)))
(test-equal? "" (+ 1 (to-int '(15 15 15 15))) (to-int '(0 0 0 0 1)))


(define successor
  (lambda (biggie)
    (let ([LIM (- N 1)])
      (letrec ([S (lambda (b)
                    (cond
                      [(null? b) '(1)]
                      [(< (car b) LIM) (cons (+ (car b) 1) (cdr b))]
                      [else (cons 0 (successor (cdr b)))]))])
        (S biggie)))))

(test-equal? "" (successor '()) '(1))
(test-equal? "" (successor '(1)) '(2))
(test-equal? "" (successor '(14)) '(15))
(test-equal? "" (successor '(15)) '(0 1))
(test-equal? "" (successor '(15 1)) '(0 2))
(test-equal? "" (successor '(15 15 1 1)) '(0 0 2 1))
(test-equal? "" (successor '(15 15 15 15)) '(0 0 0 0 1))

(define predecessor
  (lambda (biggie)
    (let ([LIM (- N 1)])
      (letrec ([P (lambda (b)
                    (cond
                      [(equal? b '(1)) (zero)]
                      [(> (car b) 0) (cons (- (car b) 1) (cdr b))]
                      [else (cons LIM (P (cdr b)))]))])
        (P biggie)))))

(test-equal? "" (predecessor '(1))         '())
(test-equal? "" (predecessor '(2))         '(1))
(test-equal? "" (predecessor '(15))        '(14))
(test-equal? "" (predecessor '(0 1))       '(15))
(test-equal? "" (predecessor '(0 2))       '(15 1))
(test-equal? "" (predecessor '(0 0 2 1))   '(15 15 1 1))
(test-equal? "" (predecessor '(0 0 0 0 1)) '(15 15 15 15))

(define plus
  (lambda (a b)
    (cond
      [(is-zero? a) b]
      [(plus (predecessor a) (successor b))])))

(test-equal? "" (plus '(4)  '(2)) '(6))
(test-equal? "" (plus '(15) '(2)) '(1 1))

(define multi
  (lambda (a b)
    (cond
      [(is-zero? a) (zero)]
      [(plus b (multi (predecessor a) b))])))

(test-equal? "" (multi '(3) '(2)) '(6))
(test-equal? "" (multi '(8) '(2)) '(0 1))
       
(define factorial
  (lambda (n)
    (cond
      [(is-zero? n) '(1)]
      [(multi n (factorial (predecessor n)))])))

(test-equal? "" (factorial '(3)) '(6))
(test-equal? "" (to-int (factorial '(8))) 40320)
; (test-equal? "" (factorial '(10)) '(0 0 15 5 7 3)) ; TAKES A LONG TIME

;(time (to-int (factorial '(6))))
;(time (to-int (factorial '(7))))
;(time (to-int (factorial '(8))))
;(time (to-int (factorial '(9))))
;(time (to-int (factorial '(10))))

; Base N       32      16       2
;          ------- -------  ------
;real time:     1       0       0
;real time:     2       3       3
;real time:    14      13      23
;real time:   173     142     280
;real time: 9_796   8_474  11_212

; 2.2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; I do not care

; 2.3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)

; 1. Every number has inifinitely representations because for any number n can be expressed as
; the difference between itself and zero
; (n) => (diff (n) (diff (one) (one))
; where recursively we can substitute (one) at ad-infinitum with the following rule:
; (one) => (diff (one) (diff (one) (one)))

(define diff
  (lambda (a b)
    (cons 'diff (cons a (cons b '())))))

(define one
  (lambda ()
    '(one)))

(define zerod
  (lambda ()
    (diff (one) (one))))

(define is-zerod?
  (lambda (t)
    (cond
      [(equal? (car t) 'diff) (equal? (car (cdr t)) (car (cdr (cdr t))))]
      [else #f])))

(test-true "" (is-zerod? (diff (one) (one))))

;(define firstD (lambda (t) (car (cdr t))))
;(define seconD (lambda (t) (car (cdr (cdr t)))))
(define firstD
  (lambda (t)
    (cond
      [(equal? t '(one)) '(one)]
      [(car (cdr t))])))

(define seconD
  (lambda (t)
    (cond
      [(equal? t '(one)) (zerod)]
      [(car (cdr (cdr t)))])))


(define to-intd
  (lambda (t)
    (cond
      [(equal? t '(one)) 1]
      [(- (to-intd (firstD t)) (to-intd (seconD t)))])))

(test-equal? "" (to-intd (diff (one) (one))) 0)
(test-equal? "" (to-intd (diff (one) (diff (one) (one)))) 1)
(test-equal? "" (to-intd (diff (diff (one) (one)) (diff (one) (zerod)))) -1)
(test-equal? "" (to-intd (diff (diff (one) (diff (one) (one)))
                                (diff (diff (one) (one)) (diff (one) (zerod))))) 2)

(define one+
  (lambda ()
    (diff
     (diff (one) (zerod))
     (diff (zerod) (diff (one) (zerod))))))

(define successord
  (lambda (t)
    (cond
      [(equal? t '(one)) (one+)]
      [(diff (successord (firstD t)) (seconD t))])))

(test-equal? "" (to-intd (successord (zerod))) 1)
(test-equal? "" (to-intd (successord (diff (one) (one)))) 1)
(test-equal? "" (to-intd (successord (successord (diff (one) (one))))) 2)
(test-equal? "" (to-intd (successord (successord (successord (diff (one) (one)))))) 3)

(define predecessord
  (lambda (t)
    (cond
      [(equal? t '(one)) (one+)]
      [(diff (firstD t) (predecessord (seconD t)))])))
    
(test-equal? "" (to-intd (predecessord (zerod))) -1)
(test-equal? "" (to-intd (predecessord (diff (one) (one)))) -1)
(test-equal? "" (to-intd (predecessord (predecessord (diff (one) (one))))) -2)
(test-equal? "" (to-intd (predecessord (predecessord (predecessord (diff (one) (one)))))) -3)

(test-equal? "" (to-intd (successord (successord (successord 
                           (predecessord (predecessord (predecessord (zerod)))))))) 0)

(define plusd_slow ; the same as plus => very slow because succ adds too much blabber
  (lambda (b1 b2)
    (cond
      [(is-zerod? b1) b2]
      [(plusd_slow (predecessord b1) (successord b2))])))

(test-equal? "" (plusd_slow (zerod) (zerod)) (zerod))

(define diff-tree-plus ; needs refactor and more thought
  (lambda (b1 b2)
    (cond
      [(is-zerod? b1) b2]
      [(is-zerod? b2) b1]
      [(equal? b1 '(one)) (diff (successord (firstD b2)) (seconD b2))]
      [(equal? b2 '(one)) (diff (successord (firstD b1)) (seconD b1))]
      [(diff (diff-tree-plus (firstD b1) (firstD b2))
             (diff-tree-plus (seconD b1) (seconD b2)))])))

(test-equal? "" (to-intd (diff-tree-plus (zerod) (zerod))) 0)
(test-equal? "" (to-intd (diff-tree-plus (one) (one))) 2)
(test-equal? "" (to-intd (diff-tree-plus (one) 
                                 (diff (diff (one) (diff (one) (one)))
                                       (diff (diff (one) (one)) (diff (one) (zerod)))))) 3)
(test-equal? "" (to-intd (diff-tree-plus (diff (diff (one) (diff (one) (one)))
                                       (diff (diff (one) (one)) (diff (one) (zerod))))
                                 (one))) 3)
(test-equal? "" (to-intd (diff-tree-plus (zerod) (diff (one) (diff (one) (one))))) 1)
(test-equal? "" (to-intd (diff-tree-plus (diff (one) (diff (one) (one))) (zerod))) 1)
(test-equal? "" (to-intd (diff-tree-plus (diff (one) (diff (one) (one)))
                                 (diff (one) (diff (one) (one))))) 2)
(test-equal? "" (to-intd (diff-tree-plus
                           (diff (diff (one) (diff (one) (one)))
                                 (diff (diff (one) (one)) (diff (one) (zerod))))                 ; 2
                           (predecessord (predecessord (predecessord (diff (one) (one))))))) -1) ; -3

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

(test-equal? "" (apply-env e 'x) 7)
(test-equal? "" (apply-env e 'y) 8)
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

(define pull            ; constructor
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

(test-false  "" (empty-stack? s))
(test-equal? "" (top s) 6)
(test-equal? "" (top (pull s)) 8)
(test-true   "" (empty-stack? (pull (pull (pull (pull s))))))

; 2.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(test-equal? "" (apply-env-al eal 'y) 8)
(check-exn exn:fail? (lambda () (apply-env-al eal 'z)))

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

(test-true  "" (apply-env-1 e1 2))
(test-false "" (apply-env-1 e1 7))

; Second Idea: List with recursive reference
;   todo: fail because of circularity not enforced !!
;   todo: the order of extensions matter !!

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

(test-equal? "" (fetch 'a '((a 1) (x c) (c 3))) 1)
(test-equal? "" (fetch 'x '((a 1) (x c) (c 3))) 3)
(test-equal? "" (fetch 'b '((a 1) (x c) (c 3))) '())

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

(test-equal? "" (apply-env-2 e2 'z) 1)
(test-equal? "" (apply-env-2 e2 'x) 1)
(test-equal? "" (apply-env-2 e2 'w) 1)
(check-exn exn:fail? (lambda () (apply-env-2 e2 'm)))

; Third Idea: Trees // Vectors // Lambda Calculus

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
      [(eqv? (car env) '(empty-env)) (report-not-found var env)]
      [(eqv? (car env) 'extend-env)
       (let ([saved-var (car (cdr env))]
             [saved-val (car (cdr (cdr env)))]
             [saved-env (car (cdr (cdr (cdr env))))])
         (cond
           [(eqv? var saved-var) saved-val]
           [(apply-env saved-env var)]))]
      [else (report-invalid-env (car env) env)])))

; 2.8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     already done in 2.5

; 2.9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     already done more extensively in 2.6 - second idea as (fetch

(define has-binding?
  (lambda (var env)
    (cond
      [(null? env) #f]
      [(equal? var (car (car env))) #t]
      [(has-binding? var (cdr env))])))

; 2.10 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(test-equal? "" e10 '((a 1) (b 2) (c 3) (m 5) (x 8) (y 9)))

; 2.11 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-env-alribs  (lambda () '()))
(define empty-env-alribs? (lambda (e) (null? e)))

(define extend-env-alribs  (lambda ( var  val env) (cons (list var val) env)))
(define extend-env-alribs* (lambda (lvar lval env) (extend-env-alribs lvar lval env)))

(define has-binding-ribs?
  (lambda (var l1 l2)
    (cond
      [(null? l1) #f]
      [(equal? var (car l1)) (car l2)]
      [else (has-binding-ribs? var (cdr l1) (cdr l2))])))

(define apply-env-alribs
  (lambda (env var)
    (if (empty-env-alribs? env) (report-not-found)
        (let ([vars (caar env)]
              [vals (car (cdr (car env)))])
          (cond
            [(list? vars)
             (or (has-binding-ribs? var vars vals)
                 (apply-env-alribs (cdr env) var))]
            [(equal? vars var) vals]
            [(apply-env-alribs (cdr env) var)])))))

(define ealribs
  (extend-env-alribs 'foo 6
    (extend-env-alribs* '(a b c) '(1 2 3)
      (extend-env-alribs 'x 7
        (extend-env-alribs* '(y z) '(8 9)
          (empty-env-alribs))))))

(test-equal? "" ealribs '((foo 6) ((a b c) (1 2 3)) (x 7) ((y z) (8 9))))
(test-equal? "" (apply-env-alribs ealribs 'foo) 6)
(test-equal? "" (apply-env-alribs ealribs 'z) 9)
(test-equal? "" (apply-env-alribs ealribs 'x) 7)
(test-equal? "" (apply-env-alribs ealribs 'z) 9)
(check-exn exn:fail? (lambda () (apply-env-al eal 'w)))

; 2.12 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-e
  (lambda ()
    (lambda (search-var)
      (report-not-found))))

(define extend-e
  (lambda (var val e)
    (lambda (search-var)
      (if (equal? search-var var)
          val
          (apply-e e search-var)))))

(define apply-e
  (lambda (e search-var)
    (e search-var)))

(define E
  (extend-e 'a 1
    (extend-e 'b 2
      (extend-e 'c 3
        (empty-e)))))

(test-equal? "" (E 'b) 2)

; Again not sure if this is correct ;;;;;;;;;;;;;;;;;;;;;;;;

(define report-end-of-stK  (lambda () (eopl:error 'whathaveyou "Enf Of Stack")))

(define empty-stK
  (lambda ()
    (lambda ()
      (report-end-of-stK))))

(define pushK
  (lambda (saved-val saved-stk)
    (lambda ()
      (pullK saved-val)
      ;saved-stk               ; if i want to return stack to keep screwing with it!
      )))

(define pullK
  (lambda (v)
    v))

(define S
  (pushK 6
    (pushK 8
      (pushK 99
        (empty-stK)))))

(define SE
  (empty-stK))

(test-equal? "" (S) 6)
(check-exn exn:fail? (lambda () (SE)))

; 2.13 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define empty-ext
  (lambda ()
     (lambda (search-var) '(() #t))))

(define extend-ext
  (lambda (var val e)
    (lambda (search-var)
      (if (equal? search-var var)
          (list val #f)
          (apply-ext e search-var)))))

(define apply-ext
  (lambda (e search-var)
    (e search-var)))

(define EX0 (empty-ext))

(define EX
  (extend-ext 'a 1
    (extend-ext 'b 2
      (extend-ext 'c 3
        (empty-ext)))))

(test-equal? "" (EX0 'b) '(() #t))
(test-equal? "" (EX  'b) '(2  #f))

; 2.14 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define has-binding-ext?
  (lambda (search-var E)
    (not (cadr (E search-var)))))

(test-false "" (has-binding-ext? 'a EX0))
(test-true  "" (has-binding-ext? 'b EX))
(test-true  "" (has-binding-ext? 'c EX))
(test-false "" (has-binding-ext? 'x EX))

; 2.15 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define occurs-free-v0?
  (lambda (V E)
    (cond
      [(symbol? E) (eqv? V E)]
      [(eqv? (car E) 'lambda)
       (and (not (eqv? (caadr E) V))
            (occurs-free-v0? V (caddr E)))]
      [else (or 
             (occurs-free-v0? V (car E))
             (occurs-free-v0? V (cadr E)))])))

; Lc-exp ::= Identifier
;        ::= (lambda (Identifier) Lc-exp)
;        ::= (Lc-exp Lc-exp)

; predicates directly from grammar
(define var-exp?     (lambda (V) (symbol? V)))
(define lambda-exp?  (lambda (E) (eqv? (car E) 'lambda)))
(define app-exp?     (lambda (E) (and (pair? E) (procedure? (car E)))))

; constructors
(define var-exp    (lambda (E) E))
(define lambda-exp (lambda (V E) (list 'lambda (V) E)))
(define app-exp    (lambda (E D) (list E D)))

; extractors
(define var-exp->var (lambda (V) V))
(define lambda-exp->bound-var (lambda (E) (car (car (cdr E)))))
(define lambda-exp->body      (lambda (E) (car (cdr (cdr E)))))
(define app-exp->rator        (lambda (E) (car E)))
(define app-exp->rand         (lambda (E) (car (cdr E))))

(define occurs-free?
  (lambda (V E)
    (cond
      ((var-exp? E) (eqv? V (var-exp->var E)))
      ((lambda-exp? E)
       (and
        (not (eqv? V (lambda-exp->bound-var E)))
        (occurs-free? V (lambda-exp->body E))))
      (else (or
             (occurs-free? V (app-exp->rator E))
             (occurs-free? V (app-exp->rand E)))))))

; 2.16 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Lc-exp ::= (lambda Identifier Lc-exp)

; the only ones to change would be:
(define lambda-exp# (lambda (V E) (list 'lambda V E)))     ; constructor
(define lambda-exp->bound-var# (lambda (E) (car (cdr E)))) ; bound var extractor

; 2.17 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; W.T.F.?? << for later >>

; 2.18 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NodeInSequence ::= (Int Listof(Int) Listof(Int))

(define number->sequence (λ(N) (list N '() '())))           ; constructor
(define current-element  (λ(NIS) (car NIS)))              ; extractor

; mine - auxiliary
(define head-seq (lambda (l) (if (null? l) (eopl:error 'head "404") (car l))))
(define tail-seq (lambda (l) (if (or (null? l) (null? (cdr l))) (eopl:error 'tail "404") (cdr l))))
(define preceding (λ(NIS) (car (cdr NIS))))               ; extractor
(define following (λ(NIS) (car (cdr (cdr NIS)))))         ; extractor

(define move-to-left
  (λ(NIS)
    (list (head-seq (preceding NIS))
          (cdr (preceding NIS))
          (cons (current-element NIS) (following NIS)))))

(define move-to-right
  (λ(NIS)
    (list (head-seq (following NIS))
          (cons (current-element NIS) (preceding NIS))
          (cdr (following NIS)))))

(define insert-to-left
  (λ(n NIS)
    (list (current-element NIS)
          (cons n (preceding NIS))
          (following NIS))))

(define insert-to-right
  (λ(n NIS)
    (list (current-element NIS)
          (preceding NIS)
          (cons n (following NIS)))))

; this is my interpretation of what left and right end are
(define at-left-end?  (λ(NIS) (null? (preceding NIS))))
(define at-right-end? (λ(NIS) (null? (following NIS))))

(test-equal? "" (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
(test-equal? "" (move-to-left '(6 (5) ())) '(5 () (6)))
(check-exn exn:fail? (lambda () (move-to-left '(6 () (7 8 9)))))

(test-equal? "" (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9)))
(test-equal? "" (move-to-right '(6 () (7))) '(7 (6) ()))
(check-exn exn:fail? (lambda () (move-to-right '(6 () ()))))

(test-equal? "" (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9)))
(test-equal? "" (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9)))

(test-true  "" (at-left-end?  '(6 () (7 8 9))))
(test-true  "" (at-right-end? '(6 (5 4 3 2 1) ())))
(test-false "" (at-left-end?  '(6 (5) (7 8 9))))
(test-false "" (at-right-end? '(6 (5 4 3 2 1) (7))))

; 2.19 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bintree ::= () | (Int Bintree Bintree)

; mine
(define fuckup (λ() (eopl:error 'fuckup "404")))
(define left-branch  (λ(t) (car (cdr t))))
(define right-branch (λ(t) (car (cdr (cdr t)))))

(define number->bintree   (λ(n) (list n '() '())))
(define current-node      (λ(t) (if (at-leaf? t) (fuckup) (car t))))
(define move-to-left-son  (λ(t) (if (at-leaf? t) (fuckup) (left-branch t))))
(define move-to-right-son (λ(t) (if (at-leaf? t) (fuckup) (right-branch t))))
(define at-leaf? (λ(t) (null? t)))

(define insert-to-left-branch
  (λ(n t) (list (current-node t)
                (list n (left-branch t) '())      ; i assume we push the tree below to the left
                (right-branch t))))

(define insert-to-right-branch
  (λ(n t) (list (current-node t)
                (left-branch t)
                (list n '() (right-branch t)))))  ; i assume we push the tree below to the right

(define t1 (insert-to-right-branch 14 (insert-to-left-branch 12 (number->bintree 13))))
(test-equal? "" (number->bintree 13) '(13 () ()))
(test-equal? "" t1 '(13 (12 () ()) (14 () ())))
(test-equal? "" (move-to-left-son t1) '(12 () ()))
(test-true   "" (at-leaf? (move-to-right-son (move-to-left-son t1))))
(test-equal? "" (insert-to-left-branch 15 t1) '(13 (15 (12 () ()) ()) (14 () ())))

; 2.20 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bintree  ::= () | (Int (Bintree Bintree))
; Memotree ::= () | tree | (Bintree Memotree)   ; stack of Bintrees
; Sototree ::= (Bintree Memotree)

(define memo->empty (λ() '()))
(define memo->push  (λ(x m) (cons x m)))
(define memo->pull  (λ(m) (cdr m)))

(define st-tree (lambda (st) (car st)))
(define st-memo (lambda (st) (car (cdr st))))
    
(define n->sototree (lambda (n) (list (number->bintree n) (memo->empty))))
(define st-node (lambda (st) (current-node (st-tree st))))

(define soto->go-left
  (lambda (st)
    (let ([T (st-tree st)] [M (st-memo st)])
      (list (move-to-left-son T)
            (memo->push (list (current-node T) #f (right-branch T)) M)))))
  
(define soto->go-right
  (lambda (st)
    (let ([T (st-tree st)] [M (st-memo st)])
      (list (move-to-right-son T)
            (memo->push (list (current-node T) (left-branch T) #f) M)))))

(define soto-add-left
  (lambda (n st)
    (list (insert-to-left-branch n (st-tree st)) (st-memo st))))

(define soto-add-right
  (lambda (n st)
    (list (insert-to-right-branch n (st-tree st)) (st-memo st))))

(define soto->go-up
  (lambda (st)
    (let ([dad (car (st-memo st))] [T (st-tree st)])
      (list
       (cons (current-node dad)
             (if (left-branch dad) (list (left-branch dad) T) (list T (right-branch dad))))
       (memo->pull (st-memo st))))))
  
(define @leaf? (λ(st) (at-leaf? (st-tree st))))
(define @root? (λ(st) (null? (st-memo st))))

(define t3 (soto-add-right 14 (soto-add-left 12 (n->sototree 13))))
(test-equal? "" t3 (list t1 (memo->empty)))
(test-equal? "" (soto->go-left t3) '((12 () ()) ((13 #f (14 () ())))))
(test-equal? "" (soto->go-up (soto->go-left t3)) t3)
(test-true   "" (@root? (soto->go-up (soto->go-right (soto->go-up (soto->go-left t3))))))

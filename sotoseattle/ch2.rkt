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

(check-equal? (to-int '()) 0)
(check-equal? (to-int '(0)) 0)
(check-equal? (to-int '(1)) 1)
(check-equal? (to-int '(1 2)) 33)
(check-equal? (to-int '(2 0 1)) 258)
(check-equal? (to-int '(18)) (to-int '(2 1)))
(check-equal? (+ 1 (to-int '(15 15 15 15))) (to-int '(0 0 0 0 1)))


(define successor
  (lambda (biggie)
    (let ([LIM (- N 1)])
      (letrec ([S (lambda (b)
                    (cond
                      [(null? b) '(1)]
                      [(< (car b) LIM) (cons (+ (car b) 1) (cdr b))]
                      [else (cons 0 (successor (cdr b)))]))])
        (S biggie)))))

(check-equal? (successor '()) '(1))
(check-equal? (successor '(1)) '(2))
(check-equal? (successor '(14)) '(15))
(check-equal? (successor '(15)) '(0 1))
(check-equal? (successor '(15 1)) '(0 2))
(check-equal? (successor '(15 15 1 1)) '(0 0 2 1))
(check-equal? (successor '(15 15 15 15)) '(0 0 0 0 1))

(define predecessor
  (lambda (biggie)
    (let ([LIM (- N 1)])
      (letrec ([P (lambda (b)
                    (cond
                      [(equal? b '(1)) (zero)]
                      [(> (car b) 0) (cons (- (car b) 1) (cdr b))]
                      [else (cons LIM (P (cdr b)))]))])
        (P biggie)))))

(check-equal? (predecessor '(1))         '())
(check-equal? (predecessor '(2))         '(1))
(check-equal? (predecessor '(15))        '(14))
(check-equal? (predecessor '(0 1))       '(15))
(check-equal? (predecessor '(0 2))       '(15 1))
(check-equal? (predecessor '(0 0 2 1))   '(15 15 1 1))
(check-equal? (predecessor '(0 0 0 0 1)) '(15 15 15 15))

(define plus
  (lambda (a b)
    (cond
      [(is-zero? a) b]
      [(plus (predecessor a) (successor b))])))

(check-equal? (plus '(4)  '(2)) '(6))
(check-equal? (plus '(15) '(2)) '(1 1))

(define multi
  (lambda (a b)
    (cond
      [(is-zero? a) (zero)]
      [(plus b (multi (predecessor a) b))])))

(check-equal? (multi '(3) '(2)) '(6))
(check-equal? (multi '(8) '(2)) '(0 1))

(define factorial
  (lambda (n)
    (cond
      [(is-zero? n) '(1)]
      [(multi n (factorial (predecessor n)))])))

(check-equal? (factorial '(3)) '(6))
(check-equal? (to-int (factorial '(8))) 40320)
; (check-equal? (factorial '(10)) '(0 0 15 5 7 3)) ; TAKES A LONG TIME

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

(check-true (is-zerod? (diff (one) (one))))

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

(check-equal? (to-intd (diff (one) (one))) 0)
(check-equal? (to-intd (diff (one) (diff (one) (one)))) 1)
(check-equal? (to-intd (diff (diff (one) (one)) (diff (one) (zerod)))) -1)
(check-equal? (to-intd (diff (diff (one) (diff (one) (one)))
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

(check-equal? (to-intd (successord (zerod))) 1)
(check-equal? (to-intd (successord (diff (one) (one)))) 1)
(check-equal? (to-intd (successord (successord (diff (one) (one))))) 2)
(check-equal? (to-intd (successord (successord (successord (diff (one) (one)))))) 3)

(define predecessord
  (lambda (t)
    (cond
      [(equal? t '(one)) (one+)]
      [(diff (firstD t) (predecessord (seconD t)))])))

(check-equal? (to-intd (predecessord (zerod))) -1)
(check-equal? (to-intd (predecessord (diff (one) (one)))) -1)
(check-equal? (to-intd (predecessord (predecessord (diff (one) (one))))) -2)
(check-equal? (to-intd (predecessord (predecessord (predecessord (diff (one) (one)))))) -3)

(check-equal? (to-intd (successord (successord (successord
                           (predecessord (predecessord (predecessord (zerod)))))))) 0)

(define plusd_slow ; the same as plus => very slow because succ adds too much blabber
  (lambda (b1 b2)
    (cond
      [(is-zerod? b1) b2]
      [(plusd_slow (predecessord b1) (successord b2))])))

(check-equal? (plusd_slow (zerod) (zerod)) (zerod))

(define diff-tree-plus ; needs refactor and more thought
  (lambda (b1 b2)
    (cond
      [(is-zerod? b1) b2]
      [(is-zerod? b2) b1]
      [(equal? b1 '(one)) (diff (successord (firstD b2)) (seconD b2))]
      [(equal? b2 '(one)) (diff (successord (firstD b1)) (seconD b1))]
      [(diff (diff-tree-plus (firstD b1) (firstD b2))
             (diff-tree-plus (seconD b1) (seconD b2)))])))

(check-equal? (to-intd (diff-tree-plus (zerod) (zerod))) 0)
(check-equal? (to-intd (diff-tree-plus (one) (one))) 2)
(check-equal? (to-intd (diff-tree-plus (one)
                                 (diff (diff (one) (diff (one) (one)))
                                       (diff (diff (one) (one)) (diff (one) (zerod)))))) 3)
(check-equal? (to-intd (diff-tree-plus (diff (diff (one) (diff (one) (one)))
                                       (diff (diff (one) (one)) (diff (one) (zerod))))
                                 (one))) 3)
(check-equal? (to-intd (diff-tree-plus (zerod) (diff (one) (diff (one) (one))))) 1)
(check-equal? (to-intd (diff-tree-plus (diff (one) (diff (one) (one))) (zerod))) 1)
(check-equal? (to-intd (diff-tree-plus (diff (one) (diff (one) (one)))
                                 (diff (one) (diff (one) (one))))) 2)
(check-equal? (to-intd (diff-tree-plus
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

(check-false  (empty-stack? s))
(check-equal? (top s) 6)
(check-equal? (top (pull s)) 8)
(check-true   (empty-stack? (pull (pull (pull (pull s))))))

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

(check-equal? (apply-env-al eal 'y) 8)
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

(check-true  (apply-env-1 e1 2))
(check-false (apply-env-1 e1 7))

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

(check-equal? e10 '((a 1) (b 2) (c 3) (m 5) (x 8) (y 9)))

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

(check-equal? ealribs '((foo 6) ((a b c) (1 2 3)) (x 7) ((y z) (8 9))))
(check-equal? (apply-env-alribs ealribs 'foo) 6)
(check-equal? (apply-env-alribs ealribs 'z) 9)
(check-equal? (apply-env-alribs ealribs 'x) 7)
(check-equal? (apply-env-alribs ealribs 'z) 9)
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

(check-equal? (E 'b) 2)

; Freaky implementation

(define report-end-of-stK  (lambda () (eopl:error 'whathaveyou "Enf Of Stack")))

(define empty-stK
  (lambda ()
    (lambda ()
      (report-end-of-stK))))

(define pushK
  (lambda (saved-val saved-stk)
    (lambda ()
      (list saved-val saved-stk))))

(define topK
  (lambda (stk)
    (car (stk))))

(define popK
  (lambda (stk)
    (car (cdr (stk)))))

(define S
  (pushK 6
    (pushK 8
      (pushK 99
        (empty-stK)))))

(define SE (empty-stK))

(check-equal? (topK (popK S)) 8)
(check-equal? (topK (popK (popK S))) 99)
(check-equal? (topK (pushK 000 (popK (popK S)))) 0)
(check-exn exn:fail? (lambda () (topK (popK (popK (popK S))))))
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

(check-equal? (EX0 'b) '(() #t))
(check-equal? (EX  'b) '(2  #f))

; 2.14 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define has-binding-ext?
  (lambda (search-var E)
    (not (cadr (E search-var)))))

(check-false (has-binding-ext? 'a EX0))
(check-true  (has-binding-ext? 'b EX))
(check-true  (has-binding-ext? 'c EX))
(check-false (has-binding-ext? 'x EX))

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

(check-equal? (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
(check-equal? (move-to-left '(6 (5) ())) '(5 () (6)))
(check-exn exn:fail? (lambda () (move-to-left '(6 () (7 8 9)))))

(check-equal? (move-to-right '(6 (5 4 3 2 1) (7 8 9))) '(7 (6 5 4 3 2 1) (8 9)))
(check-equal? (move-to-right '(6 () (7))) '(7 (6) ()))
(check-exn exn:fail? (lambda () (move-to-right '(6 () ()))))

(check-equal? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9)))
(check-equal? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9)))

(check-true  (at-left-end?  '(6 () (7 8 9))))
(check-true  (at-right-end? '(6 (5 4 3 2 1) ())))
(check-false (at-left-end?  '(6 (5) (7 8 9))))
(check-false (at-right-end? '(6 (5 4 3 2 1) (7))))

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
(check-equal? (number->bintree 13) '(13 () ()))
(check-equal? t1 '(13 (12 () ()) (14 () ())))
(check-equal? (move-to-left-son t1) '(12 () ()))
(check-true   (at-leaf? (move-to-right-son (move-to-left-son t1))))
(check-equal? (insert-to-left-branch 15 t1) '(13 (15 (12 () ()) ()) (14 () ())))

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

(define soto->add-left
  (lambda (n st)
    (list (insert-to-left-branch n (st-tree st)) (st-memo st))))

(define soto->add-right
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

(define t3 (soto->add-right 14 (soto->add-left 12 (n->sototree 13))))
(check-equal? t3 (list t1 (memo->empty)))
(check-equal? (soto->go-left t3) '((12 () ()) ((13 #f (14 () ())))))
(check-equal? (soto->go-up (soto->go-left t3)) t3)
(check-true   (@root? (soto->go-up (soto->go-right (soto->go-up (soto->go-left t3))))))

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

; 2.22 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype stack-dt stack-dt?
  (empty-stack-dt)
  (push-dt
     (val number?)
     (stack-dt stack-dt?)))

(define empty-stack-dt?
  (lambda (stk)
    (cases stack-dt stk
      (empty-stack-dt () #t)
      (else #f))))

(define top-dt
  (lambda (stk)
    (cases stack-dt stk
      (empty-stack-dt () #f)
      (push-dt (val stack-dt) val))))

(define pull-dt
  (lambda (stk)
    (cases stack-dt stk
      (empty-stack-dt () #f)
      (push-dt (val stack-dt) stack-dt))))

(define sdt
  (push-dt 6
    (push-dt 8
      (push-dt 7
        (push-dt 14
          (empty-stack-dt))))))

(check-false  (empty-stack-dt? sdt))
(check-equal? (top-dt sdt) 6)
(check-equal? (top-dt (pull-dt sdt)) 8)
(check-true   (empty-stack-dt? (pull-dt (pull-dt (pull-dt (pull-dt sdt))))))
(check-false  (empty-stack-dt? (pull-dt (pull-dt (pull-dt sdt)))))

; 2.23 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define valid-var-23?
  (lambda (x)
    (not (and (symbol? x)
              (list? x)
              (vector? x)
              (number? x)
              (boolean? x)
              (or (equal? x 'lambda) (equal? x 'λ))))))

(define-datatype lc-exp-23 lc-exp-23?
  (var-exp-23
   (var valid-var-23?))
  (lambda-exp-23
   (lolailo lambda?)                                      ; diff
   (bound-var valid-var-23?)
   (body lc-exp-23?))
  (app-exp-23
   (rator lc-exp-23?)
   (rand lc-exp-23?)))

(define lambda? (λ(x) (equal? x 'lambda)))                  ; diff

(define occurs-freak?
  (lambda (search-var exp)
    (cases lc-exp-23 exp
      (var-exp-23 (var) (eqv? var search-var))
      (lambda-exp-23 (lolailo bound-var body)             ; diff
                  (and (not (eqv? search-var bound-var))
                       (occurs-freak? search-var body)))
      (app-exp-23 (rator rand)
               (or (occurs-freak? search-var rator)
                   (occurs-freak? search-var rand))))))

; 2.24 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define bintree-to-list
  (lambda (T)
    (cases bintree T
      (leaf-node (n) (list 'leaf-node n))
      (interior-node (k l r) (list k (bintree-to-list l) (bintree-to-list r))))))

(check-equal? (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
                '(a (leaf-node 3) (leaf-node 4)))

; 2.25 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2 (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3 (interior-node 'baz tree-2 (leaf-node 1)))

(define tree-key
  (lambda (T)
    (cases bintree T
      (interior-node (k l r) k)
      (else #f))))

(define max-interior ; pretty fugly
  (lambda (T)
    (let ([max-key #f] [max-val 0])
      (letrec ((traverse
                (lambda (t)
                  (cases bintree t
                    (leaf-node (n) n)
                    (interior-node (k l r)
                                   (let ([ML (traverse l)] [MR (traverse r)])
                                     (let ([MX (max (+ ML MR) ML MR)])
                                       (cond
                                         [(equal? ML MX) (set! max-key (tree-key l))]
                                         [(equal? MR MX) (set! max-key (tree-key r))]
                                         [else (set! max-key k)])
                                       (set! max-val MX))
                                     (+ ML MR)))))))
        (traverse T)
        (list max-key max-val)))))

(check-equal? (max-interior tree-2) '(foo 5))
(check-equal? (max-interior tree-3) '(baz 5))

; 2.26 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Red-blue-tree    ::= Red-blue-subtree
; Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;                  ::= (blue-node {Red-blue-subtree}∗)
;                  ::= (leaf-node Int)

(define-datatype tree-rb tree-rb?
  (leafy-node
   (num integer?))
  (red-node
   (rbt1 tree-rb?)(rbt2 tree-rb?))
  (blue-node-single
    (rbt1 tree-rb?))
  (blue-node-multi
    (rbt1 tree-rb?)
    (blue-node-multi tree-rb?)))

(define mark-red-depth
  (lambda (tree)
    (letrec
        ([M (lambda (t n)
              (cases tree-rb t
                (leafy-node (m) (leafy-node n))
                (blue-node-single (head) (blue-node-single (M head n)))
                (blue-node-multi (head tail) (blue-node-multi (M head n) (M tail n)))
                (red-node (t1 t2)
                          (red-node (M t1 (+ n 1)) (M t2 (+ n 1))))))])
      (M tree 0))))

(define pepe
  (red-node
   (blue-node-multi
    (leafy-node 26)
    (leafy-node 12))
   (red-node
    (leafy-node 11)
    (blue-node-multi
     (leafy-node 117)
     (leafy-node 14)))))

(check-equal? (mark-red-depth pepe)
                (red-node
                 (blue-node-multi
                  (leafy-node 1)
                  (leafy-node 1))
                 (red-node
                  (leafy-node 2)
                  (blue-node-multi
                   (leafy-node 2)
                   (leafy-node 2)))))

; 2.27 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ((lambda (a) (a b)) c)
;
;                                 app-exp
;                               /         \
;                              /           \
;                             /             \
;                         rator             rand
;                           |                 |
;                     lambda-exp              |
;                    /          \          var-exp
;                   /            \            |
;                  /              \           c
;             bound var         app-exp
;                 |              /    \
;                 a             /      \
;                            rator     rand
;                              |         |
;                              |         |
;                           var-exp   var-exp
;                              |         |
;                              a         b

; (lambda (x)
;    (lambda (y)
;      ((lambda (x)
;         (x y))
; x)))
;
;                                 lambda-exp
;                                 /         \
;                                /           \
;                               /             \
;                         bound var           lambda-exp
;                             |                /        \
;                             x               /          \
;                                            /            \
;                                      bound var          app-exp
;                                          |             /       \
;                                          y            /         \
;                                                    rator       rand
;                                                     /             \
;                                                lambda-exp        var-exp
;                                                /        \           |
;                                               /          \          x
;                                              /            \
;                                        bound var        app-exp
;                                            |            /     \
;                                            x           /       \
;                                                      rator    rand
;                                                        |        |
;                                                     var-exp  var-exp
;                                                        |         |
;                                                        x         y

; 2.28 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define valid-var?
  (lambda (x)
    (not (and (symbol? x) (list? x) (vector? x) (number? x) (boolean? x))))) ; brutal, I know

(define-datatype lc-exp lc-exp?
  (Var-exp
   (var valid-var?))
  (Lambda-exp
   (bound-var valid-var?)
   (body lc-exp?))
  (App-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define unparse!
  (lambda (exp)
    (cases lc-exp exp
      (Var-exp (var) (symbol->string var))
      (Lambda-exp(bound-var body)
                  (format "(lambda (~A) (~A)" bound-var (unparse! body)))
      (App-exp (rator rand)
               (format "(~A ~A)" (unparse! rator) (unparse! rand))))))

(define parse!
  (lambda (datum)
    (cond
      ((symbol? datum) (Var-exp datum))
      ((list? datum)
       (if (eqv? (car datum) 'lambda)
           (Lambda-exp (car (cadr datum))
                       (parse! (caddr datum)))
           (App-exp
            (parse! (car datum))
            (parse! (cadr datum)))))
      (else #f))))

(define fifi
   (App-exp
    (Lambda-exp 'a (App-exp (Var-exp 'a) (Var-exp 'b)))
    (Var-exp 'c)))

(check-equal? (parse! '((lambda (a) (a b)) c)) fifi)
(check-equal? (unparse! (parse! '((lambda (a) (a b)) c))) "((lambda (a) ((a b)) c)")

; 2.29 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define listo
  (lambda (predicador)
    (lambda (val)
      (if (list? val)
          (and (predicador (car val)) ((listo predicador) (cdr val)))
          (predicador val)))))

(define-datatype λexp λexp?
  (λvar
   (var valid-var?))
  (λfun
   (bound-var (listo valid-var?))
   (body λexp?))
  (λapp
   (rator λexp?)
   (rand (listo λexp?))))

(define λunparse!
  (lambda (exp)
    (cases λexp exp
      (λvar (var) (symbol->string var))
      (λfun (bound-var body) (format "(lambda (~A) (~A)" bound-var (λunparse! body)))
      (λapp (rator rand) (format "(~A ~A)" (λunparse! rator) (λunparse! rand))))))

(define λparse!
  (lambda (datum)
    (cond
      ((symbol? datum) (λvar datum))
      ((list? datum)
       (if (eqv? (car datum) 'lambda)
           (λfun (car (cadr datum)) (λparse! (caddr datum)))
           (λapp (λparse! (car datum)) (λparse! (cadr datum)))))
      (else #f))))

(define chuchi (λapp (λfun 'a (λapp (λvar 'a) (λvar 'b))) (λvar 'c)))
(check-equal? (λparse! '((lambda (a) (a b)) c)) chuchi)
(check-equal? (λunparse! (λparse! '((lambda (a) (a b)) c))) "((lambda (a) ((a b)) c)")

; 2.30 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sorry, I do not give a shit about this exercise

; 2.31 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STUCK!!
; Prefix-list ::=(Prefix-exp)
; Prefix-exp  ::= Int
;             ::= Prefix-exp Prefix-exp

(define-datatype prefix-list prefix-list?
  (list-of-prefixes
   (head-prefix prefix-exp?)
   (rest-prefix prefix-list?)))

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define conchita
  (diff-exp
   (diff-exp
    (const-exp 3)
    (const-exp 2))
   (diff-exp
    (const-exp 4)
    (diff-exp
     (const-exp 12)
     (const-exp 7)))))

;(define prex-parse
;  (lambda (l) l
;    (cond
;      [(null? l) l]
;      [(number? l) (const-exp l)]
;      [(list? l)
;       (cond
;         [(number? (car l)) (prex-parse (car l))]
;         [(eqv? '- (car l))
;         ...
;    ))

;(check-equal? (prex-parse '(2)) (const-exp 2))
;(check-equal? (prex-parse '(- 3 2)) (diff-exp (const-exp 3) (const-exp 2)))
;(prex-parse '(- 3 - 2 4))
;(prex-parse '(- - 3 2 4))
;(prex-parse '(- - 3 2 - 4 - 12 7))
;conchita






#lang eopl
(require racket)
(require rackunit)

; 2.11 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define report-not-found (lambda () (eopl:error 'apply-env "No binding found")))

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
(check-exn exn:fail? (lambda () (apply-env-alribs ealribs 'w)))

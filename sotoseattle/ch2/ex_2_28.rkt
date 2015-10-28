#lang eopl
(require racket)
(require rackunit)

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

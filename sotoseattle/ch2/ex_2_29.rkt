#lang eopl
(require racket)
(require rackunit)

; 2.29 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define valid-var?
  (lambda (x)
    (not (and (symbol? x) (list? x) (vector? x) (number? x) (boolean? x))))) ; brutal, I know

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

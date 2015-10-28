#lang eopl
(require racket)
(require rackunit)

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

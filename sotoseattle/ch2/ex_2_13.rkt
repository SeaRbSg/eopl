#lang eopl
(require racket)
(require rackunit)

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

#lang eopl
(require racket)
(require rackunit)

; 2.12 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define report-not-found (lambda () (eopl:error 'apply-env "No binding found")))

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

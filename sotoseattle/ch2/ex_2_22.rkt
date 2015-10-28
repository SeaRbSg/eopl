#lang eopl
(require racket)
(require rackunit)

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

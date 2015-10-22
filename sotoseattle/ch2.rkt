#lang eopl
(require racket)
(require rackunit)
(require racket/trace)

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

(test-equal? "." (to-int '()) 0)
(test-equal? "." (to-int '(0)) 0)
(test-equal? "." (to-int '(1)) 1)
(test-equal? "." (to-int '(1 2)) 33)
(test-equal? "." (to-int '(2 0 1)) 258)
(test-equal? "." (to-int '(18)) (to-int '(2 1)))
(test-equal? "." (+ 1 (to-int '(15 15 15 15))) (to-int '(0 0 0 0 1)))


(define successor
  (lambda (biggie)
    (let ([LIM (- N 1)])
      (letrec ([S (lambda (b)
                    (cond
                      [(null? b) '(1)]
                      [(< (car b) LIM) (cons (+ (car b) 1) (cdr b))]
                      [else (cons 0 (successor (cdr b)))]))])
        (S biggie)))))

(test-equal? "." (successor '()) '(1))
(test-equal? "." (successor '(1)) '(2))
(test-equal? "." (successor '(14)) '(15))
(test-equal? "." (successor '(15)) '(0 1))
(test-equal? "." (successor '(15 1)) '(0 2))
(test-equal? "." (successor '(15 15 1 1)) '(0 0 2 1))
(test-equal? "." (successor '(15 15 15 15)) '(0 0 0 0 1))

(define predecessor
  (lambda (biggie)
    (let ([LIM (- N 1)])
      (letrec ([P (lambda (b)
                    (cond
                      [(equal? b '(1)) (zero)]
                      [(> (car b) 0) (cons (- (car b) 1) (cdr b))]
                      [else (cons LIM (P (cdr b)))]))])
        (P biggie)))))

(test-equal? "." (predecessor '(1))         '())
(test-equal? "." (predecessor '(2))         '(1))
(test-equal? "." (predecessor '(15))        '(14))
(test-equal? "." (predecessor '(0 1))       '(15))
(test-equal? "." (predecessor '(0 2))       '(15 1))
(test-equal? "." (predecessor '(0 0 2 1))   '(15 15 1 1))
(test-equal? "." (predecessor '(0 0 0 0 1)) '(15 15 15 15))

(define plus
  (lambda (a b)
    (cond
      [(is-zero? a) b]
      [(plus (predecessor a) (successor b))])))

(test-equal? "." (plus '(4)  '(2)) '(6))
(test-equal? "." (plus '(15) '(2)) '(1 1))

(define multi
  (lambda (a b)
    (cond
      [(is-zero? a) (zero)]
      [(plus b (multi (predecessor a) b))])))

(test-equal? "." (multi '(3) '(2)) '(6))
(test-equal? "." (multi '(8) '(2)) '(0 1))
       
(define factorial
  (lambda (n)
    (cond
      [(is-zero? n) '(1)]
      [(multi n (factorial (predecessor n)))])))

(test-equal? "." (factorial '(3)) '(6))
(test-equal? "." (to-int (factorial '(8))) 40320)
; (test-equal? "." (factorial '(10)) '(0 0 15 5 7 3)) ; TAKES A LONG TIME

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

(test-true "." (is-zerod? (diff (one) (one))))

(define firstD (lambda (t) (car (cdr t)) ))
(define seconD (lambda (t) (car (cdr (cdr t)))))

(define to-intd
  (lambda (t)
    (cond
      [(equal? t '(one)) 1]
      [(- (to-intd (firstD t)) (to-intd (seconD t)))])))

(test-equal? "." (to-intd (diff (one) (one))) 0)
(test-equal? "." (to-intd (diff (one) (diff (one) (one)))) 1)
(test-equal? "." (to-intd (diff (diff (one) (one)) (diff (one) (zerod)))) -1)
(test-equal? "." (to-intd (diff (diff (one) (diff (one) (one)))
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

(test-equal? "." (to-intd (successord (zerod))) 1)
(test-equal? "." (to-intd (successord (diff (one) (one)))) 1)
(test-equal? "." (to-intd (successord (successord (diff (one) (one))))) 2)
(test-equal? "." (to-intd (successord (successord (successord (diff (one) (one)))))) 3)

(define predecessord
  (lambda (t)
    (cond
      [(equal? t '(one)) (one+)]
      [(diff (firstD t) (predecessord (seconD t)))])))
    
(test-equal? "." (to-intd (predecessord (zerod))) -1)
(test-equal? "." (to-intd (predecessord (diff (one) (one)))) -1)
(test-equal? "." (to-intd (predecessord (predecessord (diff (one) (one))))) -2)
(test-equal? "." (to-intd (predecessord (predecessord (predecessord (diff (one) (one)))))) -3)

(test-equal? "." (to-intd (successord (successord (successord 
                           (predecessord (predecessord (predecessord (zerod)))))))) 0)











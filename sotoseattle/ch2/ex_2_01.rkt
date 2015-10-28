#lang eopl
(require racket)
(require rackunit)

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


#lang eopl
(require racket)
(require rackunit)

; 1.1

; 1. {3n + 2 | n ∈ N} => 2, 5, 8, 11, 14, ...
;
; Top Down
;   x = 2, or
;   3x - 1 ∈ S
;
; Bottom Up
;   2 ∈ S, and
;   if x ∈ S,then x + 3 ∈ S
;
; Rules of Inference
;    _______
;     2 ∈ S
;
;     x ∈ S
;   ---------
;   x + 3 ∈ S


; 2. {2n + 3m + 1 | n, m ∈ N} => 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...
;
; Top Down
;   x != 0, or
;   x != 2  or
;   x ∈ S
;
; Bottom Up
;   0 !∈ S, and
;   2 !∈ S, and
;   if x ∈ S,then x + 1 ∈ S
;
; Rules of Inference
;    _________________
;     0 !∈ S & 2 !∈ S
;
;          x ∈ S
;       ---------
;       x + 1 ∈ S

; 3. {(n, 2n + 1) | n ∈ N} => (0, 1), (1, 3), (2, 5), (3, 7), ...
;
; Top Down
;   x = 0 and y = 1, or
;   (x, 2y - 1 ) ∈ S
;
; Bottom Up
;   (0, 1) ∈ S, and
;   if (x, y) ∈ S,then (x + 1, 2y + 1) ∈ S
;
; Rules of Inference
;        ____________
;         (0, 1) ∈ S
;
;         (x, y) ∈ S
;    ---------------------
;     (x + 1, 2y + 1) ∈ S

; 4. {(n, n^2) | n ∈ N}
;
; Don't care

; 1.2

; 1 - pairs where car is n ∈ N, cadr is n ∈ N and n >= 7
; 2 - pairs where car is n ∈ N, cadr is n even
; 3 - triplets where car is n ∈ N, cadr and caddr are fibonacci numbers.
; 4 - triplets where car is n ∈ N, cadr is odd numbers, and caddr is powers of 2 (n^2)

; 1.3 Find a set T of natural numbers such that 0 ∈ T, and whenever n ∈ T,
; then n + 3 ∈ T, but T != S, where S is the set defined in definition 1.1.2:
;   1. 0 ∈ S,and
;   2. if n ∈ S, then n + 3 ∈ S

; The only thing I can think of is the set {0}

; 1.4
;    <list-of-numbers>
; => (number . <list-of-numbers>)
; => (-7 . <list-of-numbers>)
; => (-7 . (number . <list-of-numbers>))
; => (-7 . (3 . <list-of-numbers>))
; => (-7 . (3 . (number . <list-of-numbers>)))
; => (-7 . (3 . (14 . <list-of-numbers>)))
; => (-7 . (3 . (14 . ())))


; 1.5 Prove that if e is an <expression> as defined in 1.1.2, then there are the same
; number of left and right parens in e.

; <expression> ::= <identifier>
;              ::= (lambda (<identifier>) <expression>)
;              ::= (<expression> <expression>)

; - if <e> ::= <identifier> then there are no parens left or right. => HOLDS
; - if <e> ::= (lambda (<identifier>) <ex2>), by induction:
;              assuming that ex2 holds the theorem true, the parens match in <e> => HOLDS
; - if <e> ::= (<ex3> <ex4>), by induction:
;              assuming that ex3 and ex4 hold the theorem true, the parens match in <e> => HOLDS

; Our induction proof relies on:
;   Given a tree strcuture (also works with series of n ∈ N),
;   assuming that the theorem holds true for the <expi>, at level k+1 of recursion (or n+1)
;   we only have to check if it holds for one level up (level k)
;   as long as we know that the tree will terminate (indeed, in our case in an <identifier>).

; return nth element of a numbered list

(define nth-ele
  (lambda (lat n)
    (cond
      [(null? lat) ; '()]
       (eopl:error 'nth-ele "List too short by ~s elements.~%" (+ n 1))]
      [(zero? n) (car lat)]
      [(nth-ele (cdr lat) (- n 1))])))

(test-equal? "" (nth-ele '(a b c) 0) 'a)
(test-equal? "" (nth-ele '(a b c) 1) 'b)
(test-equal? "" (nth-ele '(a b c) 2) 'c)
(check-exn exn:fail? (lambda () (nth-ele '(a b c) 3)))

; 1.6
; short answer => it is a stupid question
; Robustify the previous to procedures (check for arg type and raise error)

(define nth-ele-robust
  (lambda (lat n)
    (cond
      [(not (list? lat)) (eopl:error 'nth-ele-robust  "This shit aint no list!")]
      [(null? lat) (eopl:error 'nth-ele-robust "List too short by ~s elements.~%" (+ n 1))]
      [(zero? n) (car lat)]
      [(nth-ele-robust (cdr lat) (- n 1))])))

; 1.7 Rewrite the error message so it is more informative

(define nth-ele-freaky
  (lambda (lat n)
    (cond
      [(not (list? lat)) (eopl:error 'nth-ele-freaky  "This shit aint no list!")]
      [(letrec
              ([dig-it (lambda (l m)
                         (cond
                           [(null? l) (eopl:error 'nth-ele-freaky "~s has no element ~s.~%" lat n)]
                           [(zero? m) (car l)]
                           [else (dig-it (cdr l) (- m 1))]))])
              (dig-it lat n))])))

(check-exn exn:fail? (lambda () (nth-ele-freaky '(1 2 3) 9)))
; (nth-ele-freaky '(1 2 3) 9)

(define remove-first
  (lambda (s los)
    (cond
      [(null? los) '()]
      [(eq? s (car los)) (cdr los)]
      [else (cons (car los) (remove-first s (cdr los)))])))

(test-equal? "" (remove-first 'd '(a b c))   '(a b c))
(test-equal? "" (remove-first 'a '(a b c))   '(b c))
(test-equal? "" (remove-first 'b '(a b c b)) '(a c b))

; 1.8 what happens with

(define remove-first-wacked
  (lambda (s los)
    (cond
      [(null? los) los]
      [(eq? s (car los)) (cdr los)]
      [(remove-first-wacked s (cdr los))])))

; gives the cdr of the list after the first occurrence of s
(test-equal? "" 3) (remove-first-wacked 3 '(1 2 3 5 3)))

; 1.9 

(define remobe
  (lambda (s los)
    (cond
      [(null? los) los]
      [(eq? s (car los)) (remobe s (cdr los))]
      [(cons (car los) (remobe s (cdr los)))])))

(test-equal? "" (remobe 3 '(1 2 3 4 3 2 1)) '(1 2 4 2 1))
(test-equal? "" (remobe 3 '(3 3 3 3 3 3 3)) '())

; 1.10
; Inclusive: one, the other or both hold
; Exclusive: one or the other, but not both hold

(define occurs-free?
  (lambda (var exp)
    (cond
      [(symbol? exp) (eqv? var exp)]
      [(eqv? (car exp) 'lambda) (and (not (eqv? (caadr exp) var))
                                    (occurs-free? var (caddr exp)))]
      [else (or (occurs-free? var (car exp))
                (occurs-free? var (cadr exp)))])))

(test-true  "" (occurs-free? 'x 'x))
(test-false "" (occurs-free? 'x 'y))
(test-false ""ccurs-free? 'x '(lambda (x) (x y))))
(test-true  "" (occurs-free? 'y '(lambda (x) (x y))))
(test-true  "" (occurs-free? 'x '((lambda (x) x) (x y))))
(test-true  "" (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))))

; The value of an expression with no free variables is FIXED, and called Combinator
;   examples are: identity function, application function

; <s-list>     ::= ()
;              ::= (<symbol-exp> . <s-list>)
; <symbol-exp> ::= <symbol> | <s-list>

(define subst
  (lambda (new old los)
    (cond
      [(null? los) los]
      [(list? (car los))
         (cons (subst new old (car los)) (subst new old (cdr los)))]
      [(eq? old (car los))
         (cons new (subst new old (cdr los)))]
      [(cons (car los) (subst new old (cdr los)))])))

(test-equal? "" (subst 5 3 '(1 2 3 4 5 3 3)) '(1 2 5 4 5 5 5))
(test-equal? "" (subst 'a 'b '(b (b c) (b () d))) '(a (a c) (a () d)))

; that is how I woudl do it. Now lets do it mutually recursive
; follow the grammar !!!! nothing more <==== Very Important

; <s-list>     ::= () | (<symbol-exp> . <s-list>)

(define subst-lst
  (lambda (new old los)
    (cond
      [(null? los) los]
      [(cons (subst-sex new old (car los))
             (subst-lst new old (cdr los)))])))

; <symbol-exp> ::= <symbol> | <s-list>

(define subst-sex
  (lambda (new old sex)
    (cond
      [(list? sex) (subst-lst new old sex)]
      [(eq? old sex) new]
      [else sex])))

(test-equal? "" (subst-lst 5 3 '(1 2 3 4 5 3 3)) '(1 2 5 4 5 5 5))
(test-equal? "" (subst-lst 'a 'b '(b (b c) (b () d))) '(a (a c) (a () d)))
    
; 1.11 Why? I ask my self...
; because at that point we know that se is not a symbol, so it has to be a list
; and when called subst (or subst-lst) on a list we are guaranteed to halt
; because the recursion is always on smaller pieces (car and cdr of the list)

; 1.12 Lets refactor back! inlining!

(define subst-lst2
  (lambda (new old los)
    (cond
      [(null? los) los]
      [(let ([a (car los)] [d (cdr los)])
         (cons
          (if (list? a) (subst-lst2 new old a) (if (eq? old a) new a))
          (subst-lst2 new old d)))])))

(test-equal? "" (subst-lst2 5 3 '(1 2 3 4 5 3 3)) '(1 2 5 4 5 5 5))
(test-equal? "" (subst-lst2 'a 'b '(b (b c) (b () d))) '(a (a c) (a () d)))

; 1.13 dont know what is map!! Forget it!

; Lets try it again following the grammar

; <top-level> ::= <s-list>
; <s-list>     ::= ()
;              ::= (<symbol-exp> . <s-list>)
; <symbol-exp> ::= <symbol> | <s-list>

(define notate-depth-top
  (lambda (lis)
    (notate-depth-lst lis 0)))

(define notate-depth-lst
  (lambda (los n)
    (cond
      [(null? los) los]
      [(cons
        (notate-depth-sex (car los) n)
        (notate-depth-lst (cdr los) n))])))

(define notate-depth-sex
  (lambda (sex n)
    (cond
      [(symbol? sex) (cons sex (cons n '()))]
      [(notate-depth-lst sex (+ n 1))])))

(test-equal? "" (notate-depth-top '(a (b () c) ((d)) e)) '((a 0) ((b 1) () (c 1)) (((d 2))) (e 0)))


; 1.14 pass

; 1.15

(define duple
  (lambda (n x)
    (cond
      [(zero? n) '()]
      [(cons x (duple (- n 1) x))])))

(test-equal? "" (duple 2 3) '(3 3))
(test-equal? "" (duple 3 '(ho ho)) '((ho ho) (ho ho) (ho ho)))
(test-equal? "" (duple 0 '(blah)) '())

; 1.16
; <list> ::= () | ({<pair>*})
; <pair> ::= (<symbol> <symbol>)

(define invert-pair
  (lambda (pair)
    (cons (cadr pair) (cons (car pair) '()))))

(define invert
  (lambda (lap)
    (cond
      [(null? lap) lap]
      [(cons (invert-pair (car lap)) (invert (cdr lap)))])))

(test-equal? "" (invert '((a 1) (a 2) (b 1) (b 2))) '((1 a) (2 a) (1 b) (2 b)))

; 1.17

(define down
  (lambda (los)
    (letrec
     ([wrap (lambda (x) (cons x '()))])
     (cond
       [(null? los) los]
       [(cons (wrap (car los)) (down (cdr los)))]))))

(test-equal? "" (down '(1 2 3)) '((1) (2) (3)))
(test-equal? "" (down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea))))
(test-equal? "" (down '(a (more (complicated)) object)) '((a) ((more (complicated))) (object)))

; 1.18

(define swapper
  (lambda (x y los)
    (cond
      [(null? los) los]
      [(list? (car los)) (cons (swapper x y (car los)) (swapper x y (cdr los)))]
      [(eq? x (car los)) (cons y (swapper x y (cdr los)))]
      [(eq? y (car los)) (cons x (swapper x y (cdr los)))]
      [else (cons (car los) (swapper x y (cdr los)))])))

(test-equal? "" (swapper 'a 'd '(a b c d)) '(d b c a))
(test-equal? "" (swapper 'a 'd '(a d () c d)) '(d a () c a))
(test-equal? "" (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))
(test-equal? "" (swapper 'x 'y '((x) y (w (z (x))))) '((y) x (w (z (y)))))

; 1.19

(define list-set
  (lambda (lis n x)
    (cond
      [(null? lis) lis]
      [(eq? (list-ref lis n) (car lis)) (cons x (cdr lis))]
      [else (cons (car lis) (list-set (cdr lis) (- n 1) x))])))

(test-equal? "" (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d))
(test-equal? "" (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3) '(1 5 10))

; 1.20

(define count-occurrences
  (lambda (s lis)
    (cond
      [(null? lis) 0]
      [(list? (car lis)) (+ (count-occurrences s (car lis)) (count-occurrences s (cdr lis)))]
      [(eq? s (car lis)) (+ 1 (count-occurrences s (cdr lis)))]
      [(count-occurrences s (cdr lis))])))

(test-equal? "" (count-occurrences 'x '((f x) y (((x z) x)))) 3)
(test-equal? "" (count-occurrences 'x '((f x) y (((x z) () x)))) 3)
(test-equal? "" (count-occurrences 'w '((f x) y (((x z) x)))) 0)

; 1.21

(define product
  (lambda (los1 los2)
    (letrec
        ([make-pair (lambda (a b) (cons a (cons b '())))]
         [glue (lambda (l1 l2)
                 (cond
                   [(null? l1) l2]
                   [(cons (car l1) (glue (cdr l1) l2))]))]
         [zip  (lambda (a los)
                 (cond
                   [(null? los) '()]
                   [else (cons (make-pair a (car los)) (zip a (cdr los)))]))])
      (cond
        [(null? los1) '()]
        [(glue (zip (car los1) los2)
               (product (cdr los1) los2))]))))

(test-equal? "" (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))

(test-equal? "" (product '(a b) '(x)) '((a x) (b x)))
(test-equal? "" (product '(a b) '()) '())
(test-equal? "" (product '() '(x y)) '())

; 1.22

(define filter-in
  (lambda (pred los)
    (cond
      [(null? los) los]
      [(pred (car los)) (cons (car los) (filter-in pred (cdr los)))]
      [(filter-in pred (cdr los))])))

(test-equal? "" (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
(test-equal? "" (filter-in symbol? '(a (b c) 17 foo)) '(a foo))

; 1.23 (done with vectors instead of lists because of 2nd edition)

(define vector-index
  (lambda (pred v)
    (letrec
        ([traverse (lambda (n)
                     (cond
                       [(zero? n) #f]
                       [(pred (vector-ref v n)) n]
                       [else (traverse (- n 1))]))])
         (traverse (- (vector-length v) 1)))))

(test-equal? "" (vector-index (lambda (x) (eqv? x 'c)) '#(a b c d)) 2)
(test-equal? "" (vector-ref '#(a b c) (vector-index (lambda (x) (eqv? x 'b)) '#(a b c))) 'b)

; 1.24

(define every?
  (lambda (pred los)
    (cond
      [(null? los) #t]
      [(pred (car los)) (every? pred (cdr los))]
      [else #f])))

(test-false "" (every? number? '(a b c 3 e)))
(test-true  "" (every? number? '(1 2 3 5 4)))

; 1.25

(define exists?
  (lambda (pred los)
    (cond
      [(null? los) #f]
      [(pred (car los)) #t]
      [else (exists? pred (cdr los))])))

(test-true  "" (exists? number? '(a b c 3 e)))
(test-false "" (exists? number? '(a b c d e)))

; 1.26

(define up
  (lambda (l)
    (letrec 
        ([glue (lambda (l1 l2)
                 (cond
                   [(not (list? l1)) (cons l1 l2)]
                   [(null? l1) l2]
                   [(cons (car l1) (glue (cdr l1) l2))]))])
      (cond
        [(null? l) l]
        [(glue (car l) (up (cdr l)))]))))
    

(test-equal? "" (up '((1 2) (3 4))) '(1 2 3 4))
(test-equal? "" (up '((x (y)) z)) '(x (y) z))

; 1.27

(define flat!
  (lambda (lis)
    (letrec 
        ([glue (lambda (l1 l2)
                 (cond
                   [(null? l1) l2]
                   [(cons (car l1) (glue (cdr l1) l2))]))])
      (cond
        [(null? lis) lis]
        [(list? (car lis)) (glue (flat! (car lis)) (flat! (cdr lis)))]
        [(cons (car lis) (flat! (cdr lis)))]
      ))))

(test-equal? "" (flat! '(a b c)) '(a b c))
(test-equal? "" (flat! '((a) () (b ()) () (c))) '(a b c))
(test-equal? "" (flat! '((a b) c (((d)) e))) '(a b c d e))
(test-equal? "" (flat! '(a b (() (c)))) '(a b c))

; 1.28

(define merge
  (lambda (lon1 lon2)
    (letrec
        ([insert (lambda (n lon)
                   (cond
                     [(null? lon) (cons n '())]
                     [(> (car lon) n) (cons n lon)]
                     [else (cons (car lon) (insert n (cdr lon)))]))])
      (cond
        [(null? lon1) lon2]
        [(merge (cdr lon1) (insert (car lon1) lon2))]))))

(test-equal? "" (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(test-equal? "" (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))

; 1.29

(define sorti
  (lambda (l)
    (letrec
        ([insert (lambda (n lon)
                   (cond
                     [(null? lon) (cons n '())]
                     [(> (car lon) n) (cons n lon)]
                     [else (cons (car lon) (insert n (cdr lon)))]))]
         [worker (lambda (input output)
                (cond
                  [(null? input) output]
                  [(worker (cdr input) (insert (car input) output))]))])
      (worker l '()))))

(test-equal? "" (sorti '(8 2 5 2 3)) '(2 2 3 5 8))

; 1.30

(define sortλ
  (lambda (λ l)
    (letrec
        ([insert (lambda (n lon)
                   (cond
                     [(null? lon) (cons n '())]
                     [(λ n (car lon)) (cons n lon)]
                     [else (cons (car lon) (insert n (cdr lon)))]))]
         [worker (lambda (input output)
                (cond
                  [(null? input) output]
                  [(worker (cdr input) (insert (car input) output))]))])
      (worker l '()))))


(test-equal? "" (sortλ < '(8 2 5 2 3)) '(2 2 3 5 8))
(test-equal? "" (sortλ > '(8 2 5 2 3)) '(8 5 3 2 2))

; 1.31 Bintree ::= Int | (Symbol Bintree Bintree)

(define lson  (lambda (l) (car (cdr l))))
(define rson  (lambda (l) (car (cddr l))))
(define leaf? (lambda (x) (integer? x)))
(define interior-node?
  (lambda (x)
    (and (list? x)
         (equal? 3 (length x))
         (leaf? (car x))
         (tree? (lson x))
         (tree? (rson x)))))
(define tree?         (lambda (x) (or (leaf? x) (interior-node? x))))

(define leaf
  (lambda (x)
    (if (integer? x) x (eopl:error 'leaf "Only admits integers as leaves"))))

(define interior-node
  (lambda (a b c)
    (if (symbol? a)
        (cons a (cons b (cons c '())))
        (eopl:error 'interior-node "Only admits atoms as node head"))))

(define contents-of
  (lambda (tree)
    (cond
      [(leaf? tree) tree]
      [else (cons (contents-of (lson tree))
                        (cons (contents-of (rson tree)) '()))])))

(test-equal? "" (contents-of 2) 2)
(test-equal? "" (contents-of '(1 2 3)) '(2 3))
(test-equal? "" (contents-of '(baz (bar 1 (foo 1 2)) (biz 4 5))) '((1 (1 2)) (4 5)))

; 1.32

(define double-tree
  (lambda (tree)
    (if (leaf? tree)
        (* 2 tree)
        (interior-node (car tree) (double-tree (lson tree)) (double-tree (rson tree))))))

(test-equal? "" (double-tree 2) '4)
(test-equal? "" (double-tree '(foo 1 2)) '(foo 2 4))
(test-equal? "" (double-tree '(bar 1 (foo 1 2))) '(bar 2 (foo 2 4)))
(test-equal? "" (double-tree '(baz (bar 1 (foo 1 2)) (biz 4 5))) '(baz (bar 2 (foo 2 4)) (biz 8 10)))

; 1.33

(define mark-leaves-with-red-depth
  (lambda (tree)
    (letrec
        ([M (lambda (t n)
                 (cond
                   [(leaf? t) n]
                   [(if (equal? (car t) 'red)
                        (set! n (+ n 1))
                        '())
                    (interior-node (car t) (M (lson t) n) (M (rson t) n))]))])
      (M tree 0))))

(test-equal? "" (mark-leaves-with-red-depth
                  (interior-node 'red
                                 (interior-node 'bar
                                                (leaf 26)
                                                (leaf 12))
                                 (interior-node 'red
                                                (leaf 11)
                                                (interior-node 'quux
                                                               (leaf 117)
                                                               (leaf 14)))))
             '(red (bar 1 1) (red 2 (quux 2 2))))

; 1.34

(define path
  (lambda (n bst)
    (let/cc jump
      (letrec
          ([left_branch  (lambda (l) (car (cdr l)))]
           [right_branch (lambda (l) (car (cdr (cdr l))))]
           [explore (lambda (b map)
                     (cond
                       [(null? b) '()]
                       [(eq? n (car b)) (jump (reverse map))]
                       [(explore (left_branch b) (cons 'left map))
                        (explore (right_branch b)(cons 'right map))]))])
        (explore bst '())))))

;                        14
;                      /    \
;                     /      \
;                    /        \
;                   7          26
;                    \        /  \
;                     \      /    \
;                     12    20     31
;                          /
;                         17

(test-equal? "" (path 17 '(14 (7 () (12 () ()))
                               (26 (20 (17 () ())
                                       ())
                                   (31 () ()))))
             '(right left left))

(test-equal? "" (path 14 '(14 (7 () (12 () ()))
                               (26 (20 (17 () ())
                                       ())
                                   (31 () ()))))
             '())

; 1.35

(define number-leaves
  (lambda (tree)
    (let ([n -1])
      (letrec ([N (lambda (t)
                    (cond
                      [(leaf? t) (set! n (+ n 1)) n]
                      [(interior-node (car t) (N (lson t)) (N (rson t)))]))])
        (N tree)))))


(test-equal? "" (number-leaves
                  (interior-node 'foo
                                 (interior-node 'bar
                                                (leaf 26)
                                                (leaf 12))
                                 (interior-node 'baz
                                                (leaf 11)
                                                (interior-node 'quux
                                                               (leaf 117)
                                                               (leaf 14)))))
             '(foo (bar 0 1) (baz 2 (quux 3 4))))

; 1.36

(define g
  (lambda (a b)
    (letrec ([G (lambda (l n)
                  (cond
                    [(null? l) l]
                    [(cons (list n (car (cdr (car l)))) (G (cdr l) (+ n 1)))]))])
        (G (cons a b) 0))))
           
(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(test-equal? "" (number-elements '(a b c d)) '((0 a) (1 b) (2 c) (3 d)))

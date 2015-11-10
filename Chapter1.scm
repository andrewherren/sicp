;;; First chapter of Abelson and Sussman's SICP

; Exercise 1.2
(/ (+ (+ 5 4)
      (- 2 (- 3 (+ 6 (/ 4 5)))))
      (* 3 (* (- 6 2) (- 2 7))))
; Value: -37/150

; Exercise 1.3
; First define square
(define (square x) (* x x))
; Then define sum-of-squares
(define (sum-of-squares a b)
  (+ (square a)
     (square b)))
; Then add all up into procedure
(define (f a b c)
  (cond ((and (< a b)
	      (< a c))
	 (sum-of-squares b c)
	 ((and (< b a)
	       (< b c))
	  (sum-of-squares a c))
	 ((and (< c b)
	       (< c a))
	  (sum-of-squares a b)))))

(f 1 2 3)
; Value: 13

; Exercise 1.4
; a-plus-abs-b returns a + |b|
;    by evaluating a + b if b > 0
;    and a - b           if b < 0

; Exercise 1.5
; normal-order evaluation will expand the expression
;    first, avoding evaluation of (p) and applicative-order
;    evaluation will first evaluate 0 and (p) entering
;    the endless loop before terminating when x is found
;    to be 0

; Exercise 1.6
; Newton's method for approximating square root
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(square (sqrt 1000))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))
(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

; rewrite sqrt-iter with new-if
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))
; Running (sqrt #) will trigger a maximum
;    recursion limit. The procedure new-if
;    is not special form, so it calls the
;    procedure as many times as necessary

; Exercise 1.7
; (sqrt 0.0001) returns 0.032, not 0.01
; (sqrt 500) returns 22.360679 properly
;    the procedure is better for large numbers

; sqrt implemented in block structure
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


; Exercise 1.10
; The following procedure computes a mathematical
;       function called Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; What are the values of the following expressions
; (A 1 10)
; 2^10 because the final argument, (A x (- y 1))
;       will evaluate 9 times before y = 1 and the
;       function stops at 2
; (A 2 4)
; 
; (A 3 3)

; Tree recursion
; Counting change program
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

; Exercise 1.11
; A function f is defined by the rule that 
;       f(n) = n if n<3 and 
;       f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3.
;       Write a procedure that computes f by means of a 
;       recursive process. Write a procedure that computes 
;       f by means of an iterative process.
; Recursive solution
(define (f n)
  (cond ((< n 3) n)
	(else (+ (+ (f (n-1)) (* 2 (f (n-2))))))
		 (* 3 (f (n-3))))))))



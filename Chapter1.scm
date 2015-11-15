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

; Exercise 1.12
; The following pattern of numbers is called Pascal's triangle.
;         1
;        1 1
;       1 2 1
;      1 3 3 1
;     1 4 6 4 1
(define (pascal row col)
  (cond ((= row 1) 1)
	((= row col) 1)
	((= col 1) 1)
	(else (+ (pascal (- row 1) (- col 1))
		 (pascal (- row 1) col)))))
;(pascal 1 1)
;Value: 1
;(pascal 2 1)
;Value: 1
;(pascal 2 2)
;Value: 1
;(pascal 3 1)
;Value: 1
;(pascal 3 2)
;Value: 2
;(pascal 3 3)
;Value: 1
;(pascal 4 2)
;Value: 3
;(pascal 4 3)
;Value: 3
;(pascal 5 3)

; Exercise 1.13
; Prove that Fib(n) is the closest integer to (phi^n)/sqrt(5), 
;      where phi = (1 + sqrt(5))/2. Hint: Let rho = (1 - sqrt(5))/2. 
; Use induction and the definition of the Fibonacci 
;  numbers (see section 1.2.2) 
;  to prove that Fib(n) = (phi^n - rho^n)/sqrt(5)
; 
; SOLUTION
; Base case: show for n = 0 and n = 1
;   n = 0 :==>
;      Fib(0) = (phi^0 - rho^0)/sqrt(5)
;           0 = (0 - 0)/sqrt(5)
;           0 = 0
;   n = 1 :==>
;      Fib(1) = (phi^1 - rho^1)/sqrt(5)
;           1 = ((1 + sqrt(5))/2 - (1 - sqrt(5))/2)/sqrt(5)
;           1 = ((2*(sqrt(5)))/2) / sqrt(5)
;           1 = (sqrt(5)) / sqrt(5)
;           1 = 1
; Induction case: 
;   take k >= 1 as given and assume relation is true for all n >= 0
;      Fib(k + 1) = Fib(k) + Fib(k - 1)
;      by induction hypothesis
;      Fib(k + 1) = ((phi^(k) - rho^(k))/sqrt(5)) + 
;                   ((phi^(k - 1) - rho^(k - 1))/sqrt(5))
;                 = ((phi)^(k) + (phi)^(k - 1))/sqrt(5) - 
;                   ((rho)^(k) + (rho)^(k - 1))/sqrt(5)
;                 = (((phi)^(k - 1)(phi + 1)) - ((rho)^(k - 1)(rho + 1)))/sqrt(5)
;      since phi^2 = phi + 1 and rho^2 = rho + 1
;                 = (((phi)^(k - 1)(phi^2)) - ((rho)^(k - 1)(rho^2)))/sqrt(5)
;                 = ((phi)^(k + 1) - (rho)^(k + 1))/sqrt(5)

; Exercise 1.14
; Draw the tree illustrating the process generated by the count-change 
; procedure of section 1.2.2 in making change for 11 cents. 
; What are the orders of growth of the space and number of steps used 
; by this process as the amount to be changed increases?



; Exercise 1.15
; The sine of an angle (specified in radians) can be computed by making 
;           use of the approximation sin x x if x is sufficiently small, 
;           and the trigonometric identity
;
;      sin(r) = 3sin(r/3) - 4sin^3(r/3)
;
; to reduce the size of the argument of sin. (For purposes of this 
;           exercise an angle is considered ``sufficiently small'' if 
;           its magnitude is not greater than 0.1 radians.) These
;           ideas are incorporated in the following procedures:
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0))))) 
; (a) How many times is the procedure p applied when (sine 12.15) is 
;          evaluated?
; (b) What is the order of growth in space and number of steps (as a 
;          function of a) used by the process generated by the sine 
;          procedure when (sine a) is evaluated?



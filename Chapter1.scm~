;;; First chapter of Abelson and Sussman's SICP

; Exercise 1.2
(/ (+ (+ 5 4)
      (- 2 (- 3 (+ 6 (/ 4 5)))))
      (* 3 (* (- 6 2) (- 2 7))))
;Value: -37/150

; Exercise 1.3
; First define square
(define (square x) (* x x))
; Then define sum-of-squares
(define (sum-of-squares a b)
  (+ (square a)
     (square b)))
; Then add all up into procedure
(define (best-sum-of-squares a b c)
  (cond ((and (< a b)
	          (< a c))
	     (sum-of-squares b c))
	    ((and (< b a)
	          (< b c))
	     (sum-of-squares a c))
	    ((and (< c b)
	          (< c a))
	     (sum-of-squares a b))))

(best-sum-of-squares 1 2 3)
;Value: 13

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
;Value: 3.00009155413138
(sqrt (+ 100 37))
;Value: 11.704699917758145
(sqrt (+ (sqrt 2) (sqrt 3)))
;Value: 1.7739279023207892
(square (sqrt 1000))
;Value: 1000.000369924366

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	    (else else-clause)))
(new-if (= 2 3) 0 5)
;Value: 5
(new-if (= 1 1) 0 5)
;Value: 0

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
; (sqrt 0.0001) returns 0.032, not 0.01
; (sqrt 500) returns 22.360679 properly
;    the procedure is better for large numbers

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
; 2^16 because the function first evaluates to (A 1 (A 2 3))
;       = (A 1 (A 1 (A 2 2)))
;       = (A 1 (A 1 (A 1 (A 2 1))))
;       = (A 1 (A 1 (A 1 2)))
;       = (A 1 (A 1 (A 0 (A 1 1))))
;       = (A 1 (A 1 (A 0 2)))
;       = (A 1 (A 1 (2 * 2)))
;       = (A 1 (A 1 4))
;       = (A 1 (A 0 (A 1 3)))
;       = (A 1 (A 0 (A 0 (A 1 2))))
;       = (A 1 (A 0 (A 0 (A 1 2))))
;       = (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;       = (A 1 (A 0 (A 0 (A 0 2))))
;       = (A 1 (A 0 (A 0 (2 * 2))))
;       = (A 1 (A 0 (A 0 4)))
;       = (A 1 (A 0 (2 * 4)))
;       = (A 1 (A 0 8))
;       = (A 1 (2 * 8))
;       = (A 1 16)
;       which as we saw above will evaluate to 2^16
; (A 3 3)
; 2^16 because the function first evaluates to (A 2 (A 3 2))
;       = (A 2 (A 2 (A 3 1)))
;       = (A 2 (A 2 2))
;       = (A 2 (A 1 (A 2 1)))
;       = (A 2 (A 1 2))
;       = (A 2 (A 0 (A 1 1)))
;       = (A 2 (A 0 2))
;       = (A 2 (2 * 2))
;       = (A 2 4)
;       which as we saw above will evaluate to 2^16
; Consider the following procedures, where A is the procedure defined above:
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))
; Give concise mathematical definitions for the functions computed by the
; procedures f, g, and h for positive integer values of n. For example,
; (k n) computes 5n2.
; f(n) = 2*n
; g(n) = 2^n
; h(n) = 2^(n^2)

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
	     (else (+ (+ (f (- n 1)) (* 2 (f (- n 2)))) (* 3 (f (- n 3)))))))
; Iterative solution
; a <- a + 2*b + 3*c
; b <- a
; c <- b
(define (f n)
  (f-iter 2 1 0 n))
(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter (+ a (* b 2) (* c 3)) a b (- count 1))))

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
(pascal 1 1)
;Value: 1
(pascal 2 1)
;Value: 1
(pascal 2 2)
;Value: 1
(pascal 3 1)
;Value: 1
(pascal 3 2)
;Value: 2
(pascal 3 3)
;Value: 1
(pascal 4 2)
;Value: 3
(pascal 4 3)
;Value: 3
(pascal 5 3)
;Value: 6

; Exercise 1.13
; Prove that Fib(n) is the closest integer to (phi^n)/sqrt(5),
;      where phi = (1 + sqrt(5))/2. Hint: Let rho = (1 - sqrt(5))/2.
; Use induction and the definition of the Fibonacci
;  numbers (see section 1.2.2)
;  to prove that Fib(n) = (phi^n - rho^n)/sqrt(5)
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
; Orders of growth: O(n^2)

; Exercise 1.15
; The sine of an angle (specified in radians) can be computed by making
;           use of the approximation sin x = x if x is sufficiently small,
;           and the trigonometric identity
;
;      sin(r) = 3sin(r/3) - 4sin^3(r/3)
;
; to reduce the size of the argument of sin. (For purposes of this
;           exercise an angle is considered "sufficiently small" if
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
; 5 times
; (b) What is the order of growth in space and number of steps (as a
;          function of a) used by the process generated by the sine
;          procedure when (sine a) is evaluated?
;

; Exercise 1.16.  Design a procedure that evolves an iterative exponentiation
;          process that uses successive squaring and uses a logarithmic number
;          of steps, as does fast-expt. (Hint: Using the observation that
;          (b^(n/2))^2 = (b^2)^(n/2), keep, along with the exponent n and the
;          base b, an additional state variable a, and define the state
;          transformation in such a way that the product a bn is unchanged from
;          state to state. At the beginning of the process a is taken to be 1,
;          and the answer is given by the value of a at the end of the process.
;          In general, the technique of defining an invariant quantity that
;          remains unchanged from state to state is a powerful way to think
;          about the design of iterative algorithms.)
; Iterative solution
; a <- b^2
; b <- a
(define (fast-expt b n)
  (fast-expt-iter 1 b n))
(define (fast-expt-iter a base count)
  (if (= count 0)
    a
    (cond ((even? count) (fast-expt-iter a (square base) (/ count 2)))
          (else (fast-expt-iter (* a base) base (- count 1))))))

; Exercise 1.17.  The exponentiation algorithms in this section are based on
;          performing exponentiation by means of repeated multiplication. In a
;          similar way, one can perform integer multiplication by means of
;          repeated addition. The following multiplication procedure (in
;          which it is assumed that our language can only add, not multiply)
;          is analogous to the expt procedure:
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
;          This algorithm takes a number of steps that is linear in b. Now
;          suppose we include, together with addition, operations double,
;          which doubles an integer, and halve, which divides an (even)
;          integer by 2. Using these, design a multiplication procedure
;          analogous to fast-expt that uses a logarithmic number of steps.
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (/ b 2))))
        (else (+ a (fast-mult a (- b 1))))))
(define (even? n)
  (= (remainder n 2) 0))
(define (double a)
  (* 2 a))

; Exercise 1.18. Using the results of exercises 1.16 and 1.17, devise a
;          procedure that generates an iterative process for multiplying two
;          integers in terms of adding, doubling, and halving and uses a
;          logarithmic number of steps.
(define (fast-mult a b)
  (fast-mult-iter 0 a b))
(define (fast-mult-iter answer a b)
  (cond ((= b 0) answer)
        ((even? b) (fast-mult-iter answer (double a) (/ b 2)))
        (else (fast-mult-iter (+ a answer) a (- b 1)))))
(define (even? n)
  (= (remainder n 2) 0))
(define (double a)
  (* 2 a))

; Exercise 1.19. There is a clever algorithm for computing the Fibonacci
;          numbers in a logarithmic number of steps. Recall the transformation
;          of the state variables a and b in the fib-iter process of section
;          1.2.2: a a + b and b a. Call this transformation T, and observe
;          that applying T over and over again n times, starting with 1 and 0,
;          produces the pair Fib(n + 1) and Fib(n). In other words, the
;          Fibonacci numbers are produced by applying Tn, the nth power of
;          the transformation T, starting with the pair (1,0). Now consider T
;          to be the special case of p = 0 and q = 1 in a family of
;          transformations Tpq, where Tpq transforms the pair (a,b) according
;          to a bq + aq + ap and b bp + aq. Show that if we apply such a
;          transformation Tpq twice, the effect is the same as using a single
;          transformation Tp'q' of the same form, and compute p' and q' in
;          terms of p and q. This gives us an explicit way to square these
;          transformations, and thus we can compute Tn using successive
;          squaring, as in the fast-expt procedure. Put this all together
;          to complete the following procedure, which runs in a logarithmic
;          number of steps
; (define (fib n)
;   (fib-iter 1 0 0 1 n))
; (define (fib-iter a b p q count)
;   (cond ((= count 0) b)
;         ((even? count)
;          (fib-iter a
;                    b
;                    <??>      ; compute p'
;                    <??>      ; compute q'
;                    (/ count 2)))
;         (else (fib-iter (+ (* b q) (* a q) (* a p))
;                         (+ (* b p) (* a q))
;                         p
;                         q
;                         (- count 1)))))

; Tpq(a,b) => (bq + aq + ap, bp + aq)
; Tpq(Tpq(a,b)) => Tpq(bq + aq + ap, bp + aq)
;               => ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
;                   (bp + aq)p + (bq + aq + ap)q)
;               => (bpq + aq^2 + bq^2 + aq^2 + apq + bqp + aqp + ap^2,
;                   bp^2 + aqp + bq^2 + aq^2 + apq)
;               => (2aq^2 + bq^2 + 2apq + 2bqp + ap^2,
;                   bp^2 + 2aqp + bq^2 + aq^2)
;               => (b(q^2 + 2qp) + a(q^2 + 2qp) + a(q^2 + p^2),
;                   b(p^2 + q^2) + a(q^2 + 2qp))
; q <- q^2 + 2qp
; p <- q^2 + p^2
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square q) (square p))
                   (+ (square q) (* 2 q p))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; Exercise 1.20. The process that a procedure generates is of course dependent
;          on the rules used by the interpreter. As an example, consider the
;          iterative gcd procedure given above. Suppose we were to interpret
;          this procedure using normal-order evaluation, as discussed in
;          section 1.1.5. (The normal-order-evaluation rule for if is
;          described in exercise 1.5.) Using the substitution method (for
;          normal order), illustrate the process generated in evaluating
;          (gcd 206 40) and indicate the remainder operations that are
;          actually performed. How many remainder operations are actually
;          performed in the normal-order evaluation of (gcd 206 40)? In
;          the applicative-order evaluation?

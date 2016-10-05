## [Section 1.1 - Elements of Programming](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1)

## Exercises
**Exercise 1.1** Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.
```scheme
10
;Value: 10
(+ 5 3 4)
;Value: 12
(- 9 1)
;Value: 8
(/ 6 2)
;Value: 3
(+ (* 2 4) (- 4 6))
;Value: 6
(define a 3)
;Value: a
(define b (+ a 1))
;Value: b
(+ a b (* a b))
;Value: 19
(= a b)
;Value: #f
(if (and (> b a) (< b (* a b)))
    b
    a)
;Value: 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;Value: 16
(+ 2 (if (> b a) b a))
;Value: 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;Value: 16
```

**Exercise 1.2** Translate the following expression into prefix form  
(5 + 4 + (2 - (3 - (6 + 4/5)))) /  
(3 (6 - 2) (2 - 7))
```scheme
(/ (+ (+ 5 4)
      (- 2 (- 3 (+ 6 (/ 4 5)))))
      (* 3 (* (- 6 2) (- 2 7))))
;Value: -37/150
```

**Exercise 1.3** Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
```scheme
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

; Run the result
(best-sum-of-squares 1 2 3)
;Value: 13
```

**Exercise 1.4** Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:
```scheme
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
```
>a-plus-abs-b returns a + |b| by evaluating:  
&nbsp;&nbsp;&nbsp;&nbsp;a + b if b > 0, and  
&nbsp;&nbsp;&nbsp;&nbsp;a - b if b < 0  

**Exercise 1.5** Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:
```scheme
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
```
Then he evaluates the expression
```scheme
(test 0 (p))
```
What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer.

>Normal-order evaluation will expand the expression first, avoding evaluation of (p) and applicative-order evaluation will first evaluate 0 and (p) entering the endless loop before terminating when x is found to be 0

**Exercise 1.6** Alyssa P. Hacker doesn't see why if needs to be provided as a special form. "Why can't I just define it as an ordinary procedure in terms of cond?" she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:
```scheme
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
```
Eva demonstrates the program for Alyssa:
```scheme
(new-if (= 2 3) 0 5)
; Value: 5
(new-if (= 1 1) 0 5)
; Value: 0
```
Delighted, Alyssa uses new-if to rewrite the square-root program:
```scheme
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
```
What happens when Alyssa attempts to use this to compute square roots? Explain.

>Running (sqrt #) will trigger a maximum recursion limit. The procedure new-if is not special form, so it calls the procedure as many times as necessary

**Exercise 1.7** The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers?

```scheme
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
(sqrt 0.0001)
; Value: 0.032
(sqrt 500)
; Value: 22.360679
```
>The procedure is better for large numbers

**Exercise 1.8** Newton's method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value  
((r/(y^2)) + 2y) / 3  
Use this formula to implement a cube-root procedure analogous to the square-root procedure.

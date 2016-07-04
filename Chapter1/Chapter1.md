## Exercises
**Exercise 1.3** Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
```scheme
; Define square
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

a-plus-abs-b returns a + |b| by evaluating:
    a + b if b > 0, and
    a - b if b < 0

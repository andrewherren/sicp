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

; Exercise 1.4 a plus abs b
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Exercise 1.6 new-if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; sqrt-iter using new-if
(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
    (average guess (/ x guess)))
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

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


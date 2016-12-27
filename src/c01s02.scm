;; ======================================================
;; Load definitions and functions from earlier chapters
;; ======================================================
(load "c01s01.scm")
;; ======================================================

; Exercise 1.10 Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; Exercise 1.11 - A function f is defined by the rule that
;        f(n) = n if n < 3 and
;        f(n) = f(n-1) + f(n-2) + f(n-3) if n >= 3
; Write a procedure that computes f by means of a recursive process.
; Write a procedure that computes f by means of an iterative process.
;
; Recursive solution
(define (f n)
  (cond ((< n 3) n)
         (else (+ (+ (f (- n 1)) (* 2 (f (- n 2))))
               (* 3 (f (- n 3)))))))
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

; Exercise 1.12 - Pascal's triangle
(define (pascal row col)
  (cond ((= row 1) 1)
        ((= row col) 1)
        ((= col 1) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

; Exercise 1.15 - sine function
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; Exercise 1.16 - iterative exponentiation
; define the iterative algorithm
(define (rapid-expt-iter b n product)
    (cond ((= n 0) product)
          ; use successive squaring for even powers greater than 2
          ((and (even? n) (> n 2)) (rapid-expt-iter b (/ n 2) (* product (* b b))))
          ; else use simple iteration
          (else (rapid-expt-iter b (- n 1) (* product b)))))
; call iterative algorithm in a shorter-form function which initializes a to be 1
(define (rapid-expt b n)
    (rapid-expt-iter b n 1))

;;; First chapter of Abelson Sussman's SICP

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
  (cond ((and (a < b)
	      (a < c))
	 (sum-of-squares b c))
	((and (b < a)
	      (b < c))
	 (sum-of-squares a c))
	((and (c < b)
	      (c < a))
	 (sum-of-squares a b))))

(f 1 2 3)
; Value: 13



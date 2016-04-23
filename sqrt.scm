; Compute the approximate square root of a given number using successive
; approximation.

(define (sqrt value)
  (define (is-good-enough? guess value)
    (< (abs (- (* guess guess) value)) 0.000001))

  (define (try guess value)
    (if (is-good-enough? guess value)
	guess
	(try (/ (+ guess (/ value guess)) 2) value)))

  (try 1 value))

(sqrt 4.0)

(sqrt 6.0)

(sqrt 9.0)
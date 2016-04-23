# Chapter 1

Chapter 1 introduces programming procedures and how they can be used to build abstractions

```scheme
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
```

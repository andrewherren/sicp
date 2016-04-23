# Chapter 1

Chapter 1 introduces programming procedures and how they can be used to build abstractions

[1. Building Abstractions with Procedures](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-9.html#%_chap_1)
 [1.1 The Elements of Programming](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1)
  [1.1.1 Expressions](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.1)
  [1.1.2 Naming and the Environment](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.2)
  [1.1.3 Evaluating Combinations](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.3)
  [1.1.4 Compound Procedures](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.4)
  [1.1.5 The Substitution Model for Procedure Application](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.5)
  [1.1.6 Conditional Expressions and Predicates](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.6)
  [1.1.7 Example: Square Roots by Newton's Method](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.7)
  [1.1.8 Procedures as Black-Box Abstractions](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1.8)
 [1.2 Procedures and the Processes They Generate](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2)
  [1.2.1 Linear Recursion and Iteration](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.1)
  [1.2.2 Tree Recursion](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.2)
  [1.2.3 Orders of Growth](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.3)
  [1.2.4 Exponentiation](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.4)
  [1.2.5 Greatest Common Divisors](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.5)
  [1.2.6 Example: Testing for Primality](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.6)
 [1.3 Formulating Abstractions with Higher-Order Procedures](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3)
  [1.3.1 Procedures as Arguments](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.1)
  [1.3.2 Constructing Procedures Using Lambda](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.2)
  [1.3.3 Procedures as General Methods](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.3)
  [1.3.4 Procedures as Returned Values](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3.4)

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
;Value: 13
```

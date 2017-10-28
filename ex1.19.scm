(define (fib n)
  (define (fib-expt-iter a b p q n)
    (cond ((= n 0) b)
	  ((even? n)
	   (fib-expt-iter a
			  b
			  (+ (* p p) (* q q))
			  (+ (* q q) (* 2 p q))
			  (/ n 2)))
	  (else
	   (fib-expt-iter (+ (* b q) (* a q) (* a p))
			  (+ (* b p) (* a q))
			  p
			  q
			  (- n 1)))))
  (fib-expt-iter 1 0 0 1 n))

(fib 5)
(fib 7)
(fib 9)
(fib 13)
(fib 12)

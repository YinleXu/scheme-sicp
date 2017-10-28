(define (fast-expt-iter b n)
  (define (iter b n a)
    (cond ((= n 0) a)
	  ((= (remainder n 2) 0)
	   (iter (square b) (/ n 2) a))
	  (else
	   (iter b (- n 1) (* a b)))))
  (iter b n 1))

(fast-expt-iter 2 7)
(fast-expt-iter 2 8)
(fast-expt-iter 2 13)
(fast-expt-iter 2 9)
(fast-expt-iter 2 18)

(define us-coins (list 50 25 10 5 1))

(define eu-coins (list 200 100 50 20 10 5 2 1))

(define no-more? null?)

(define (except-first-denomination cl)
  (cdr cl))

(define (first-denomination cl)
  (car cl))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (negative? amount) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(cc 17 us-coins)

(cc 17 eu-coins)

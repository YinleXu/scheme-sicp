(square 2)
(append (append '() (list 1)) (list 2))

(define (square-list ls)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer
		      (list (square (car things)))))))
  (iter ls '()))

(square-list (list 1 2 3 4 5 6))

(define (count-leaves tree)
  (accumulate + 0 (map (lambda (node)
			 (if (pair? node)
			     (count-leaves node)
			     1))
		       tree)))

(count-leaves '(2 2 (2 2 (2) 2) 2 (2) 2))

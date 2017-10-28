(define (fringe ls)
  (cond ((null? ls) '())
	((not (pair? ls)) (list ls))
	(else
	 (append (fringe (car ls))
		 (fringe (cdr ls))))))

(define x (list (list 1 3) (list 2 4)))

(fringe x)

(fringe (cons x x))

(fringe (append x x))

(fringe (list x x))

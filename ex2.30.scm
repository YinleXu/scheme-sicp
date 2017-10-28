(define (square-tree-rec tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else
	 (cons (square-tree-rec (car tree))
	       (square-tree-rec (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (t)
	 (if (pair? t)
	     (square-tree-map t)
	     (square t)))
       tree))

(define list1 '(1 (2 (3 4) 5) (6 7)))

(square-tree-rec list1)

(square-tree-map list1)

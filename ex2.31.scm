(define (tree-map func tree)
  (map (lambda (t)
	 (if (pair? t)
	     (tree-map func t)
	     (func t)))
       tree))

(define (square-tree tree) (tree-map square tree))

(define x '(1 (2 (3 4) 5) (6 7)))

(square-tree x)

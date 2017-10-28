(define (subsets s)
  (if (null? s)
      '(())
      (let ((ss (subsets (cdr s))))
	(append ss
		(map (lambda (x) (append (list (car s)) x))
		     ss)))))

(define set1 '(1 2 3))

(subsets set1)

(append '() '(1 2))

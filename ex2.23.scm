(define (for-each func ls)
  (if (null? ls)
      #t
      (begin (func (car ls))
	     (for-each func (cdr ls)))))

(for-each (lambda (x)
	    (newline)
	    (display x))
	  (list 1 3 4 5 6))

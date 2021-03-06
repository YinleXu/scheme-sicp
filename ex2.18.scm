(define (reverse ls)
  (if (null? ls)
      '()
      (append (reverse (cdr ls))
	      (list (car ls)))))

(cons 3 (cons 4 (cons 5 '())))

(reverse (append '() (list 5)))

(reverse '())

(reverse (list 1 3 5 7 2 4 6 6))

(reverse '(1 2 3 (5 6 7) 8 (9)))

(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
	 (accumulate op initial (cdr sequence)))))

(define (map op sequence)
  (accumulate (lambda (x y)
		(cons (op x) y))
	      '()
	      sequence))

(map square (list 1 2 3 4))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 3 2 1) (list 1 2 3))

(define (length sequence)
  (accumulate (lambda (x y)
		(+ 1 y))
	      0
	      sequence))

(length (list 2 2 2 3 3))

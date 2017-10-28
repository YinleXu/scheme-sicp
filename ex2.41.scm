(define (unique-triples-dumb n)
  (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list i j k))
						 (enumerate-interval 1 (- j 1))))
				(enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(unique-triples-dumb 5)

(define (unique-triples n)
  (flatmap (lambda (i) (map (lambda (pair) (cons i pair))
			(unique-pairs-flatmap (- i 1))))
       (enumerate-interval 3 n)))

(unique-triples 5)

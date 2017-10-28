(define (same-parity a . b)
  (let ((pari (remainder a 2)))
    (define (rec-func ls)
      (if (null? ls)
	  '()
	  (if (= (remainder (car ls) 2) pari)
	      (cons (car ls) (rec-func (cdr ls)))
	      (rec-func (cdr ls)))))
    (cons a (rec-func b))))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7 8)

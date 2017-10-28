(define (square-list ls)
  (if (null? ls)
      '()
      (cons (* (car ls) (car ls))
	    (square-list (cdr ls)))))

(square-list (list 1 2 3 4 5 6))
(square-list '())
(square-list (list 1))

(define (square-list ls)
  (map (lambda (x) (* x x))
       ls))

(square-list (list 1 2 3 4 5 6))
(square-list '())
(square-list (list 1))

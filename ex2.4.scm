(define (cons x y)
  (lambda (m) (m x y)))

(define (car p)
  (p (lambda (x y) x)))

(define (cdr p)
  (p (lambda (x y) y)))


(define a (cons 4 5))

(car a)

(cdr a) 

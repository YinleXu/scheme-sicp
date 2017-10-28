(expt 2 10)
(remainder 9 2)

(define (number-of-factor product factor)
  (define (iter-proc prod fac n)
    (if (> (remainder prod fac) 0)
	n
	(iter-proc (/ prod fac) fac (+ n 1))))
  (iter-proc product factor 0))

(number-of-factor 1024 2)

(number-of-factor 3072 2)

(number-of-factor 3072 3)


(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car p)
  (number-of-factor p 2))

(define (cdr p)
  (number-of-factor p 3))

(define a (cons 5 2))

(car a)

(cdr a)

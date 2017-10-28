(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess tolerance)
  (define (good-enough? a b)
    (< (abs (- a b)) tolerance))

  (define (try-guess guess)
    (let ((new-guess (f guess)))
      (if (good-enough? guess new-guess)
	  new-guess
	  (try-guess new-guess))))
  
  (try-guess first-guess))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0 0.0001))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

((compose sqrt sqrt) 16)

(define (repeated-rec f n)
  (cond ((= n 0) (lambda (x) x))
	((even? n) (double (repeated-rec f (/ n 2))))
	(else
	 (compose f (repeated-rec f (- n 1))))))

(define (square x)
  (* x x))

((repeated-rec square 4) 2)
	 
(define (repeated-iter f n)
  (define (iter r tmp m)
    (cond ((= m 0) r)
	  ((even? m) (iter r (double tmp) (/ m 2)))
	  (else (iter (compose tmp r) tmp (- m 1)))))

  (iter (lambda (x) x) f n))

((repeated-iter square 3) 2)

(define (fixed-point-with-transform f trans guess tolerance)
  (fixed-point (trans f) guess tolerance))

(define (4throot x)
  (fixed-point-with-transform (lambda (y) (/ x (* y (* y y))))
			      (double average-damp)
			      1.0
			      0.00001))
(4throot 256)

(define (^ a n)
  (define (iter r tmp i)
    (cond ((= i 0) r)
	  ((even? i) (iter r (square tmp) (/ i 2)))
	  (else (iter (* r tmp) tmp (- i 1)))))
  (iter 1 a n))

(^ 2 10)

(define (logb b x)
  (/ (log x) (log b)))

(floor (logb 2 712))

(define (nthroot x n)
  (fixed-point-with-transform (lambda (y) (/ x (^ y (- n 1))))
			      (repeated-iter average-damp 
					     (floor (logb 2 n)))
			      1.0
			      0.000001))

(nthroot 32 5)

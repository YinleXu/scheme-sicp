(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d)))
	(signed-n (if (negative? d) (- n) n))
	(signed-d (abs d)))
    (cons (/ signed-n g)
	  (/ signed-d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (display-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define rat1 (make-rat -3 -6))

(display-rat rat1)


(define (proc-cons a b)
  (lambda (choice)
    (if (= choice 1)
	a
	b)))

(define (proc-car proc-pair)
  (proc-pair 1))

(define (proc-cdr proc-pair)
  (proc-pair 2))


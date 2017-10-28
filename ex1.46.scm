(define (iterative-improve good-enough? improve)
  ((lambda (x) (x x))
   (lambda (self-rec)
     (lambda (guess)
       (let ((new-guess (improve guess)))
	 (if (good-enough? guess new-guess)
	     new-guess
	     ((self-rec self-rec) new-guess)))))))

(define (average-damp f)
  (lambda (x) (/ (+ (f x) x) 2)))

(define (sqrt x)
  (let ((precision 0.000001))
    ((iterative-improve (lambda (g ng) (< (abs (- g ng)) precision))
			(average-damp (lambda (y) (/ x y))))
     1.0)))

(sqrt 3)

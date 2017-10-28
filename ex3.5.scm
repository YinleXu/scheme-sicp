(define (random-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-left trials-passed)
    (if (= trials-left 0)
	trials-passed
	(if (experiment)
	    (iter (- trials-left 1) (+ trials-passed 1))
	    (iter (- trials-left 1) trials-passed))))

  (iter trials 0))

(define (estimate-integeral P xl xu yl yu n)
  (define (P?)
    (P (random-range xl xu) (random-range yl yu)))
  (* (- xu xl)
     (- yu yl)
     (/ (monte-carlo n p?) n)))

(estimate-integeral
 (lambda (x y) 
   (<= (+ (square x) (square y))
       1))
 -1.0
 1.0
 -1.0
 1.0
 500000.0)



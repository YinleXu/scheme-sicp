(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs-flatmap n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	  
(unique-pairs-flatmap 5)

(define (make-pair-sum pair)
  (list (car pair) (cdr pair) (+ (car pair) (cadr pair))))

(prime-sum-pairs 6)

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(prime-sum-pairs 6)



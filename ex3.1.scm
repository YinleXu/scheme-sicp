(define (make-accumulator base)
  (lambda (amount)
    (begin 
      (set! base (+ base amount))
      base)))

(define A (make-accumulator 10))

(A 5)
(A 10)

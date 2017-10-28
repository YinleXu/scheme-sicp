(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))


(define (f x)
  (+ x 1))
(define x 0)

(define a0 ((zero f) x))
a0

(define a1 ((one f) x))
a1

(define a1-by-add1 (((add-1 zero) f) x))
a1-by-add1

(define a2 ((two f) x))
a2

(define a3-by-add1 (((add-1 two) f) x))
a3-by-add1

(define three (add-1 two))

(define five-by-plus (plus two three))
((five-by-plus f) x)

(define ten-by-plus (plus five-by-plus five-by-plus))
((ten-by-plus f) x)

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-rec term a next b)
  (accumulate-rec + 0 term a next b))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-rec term a next b)
  (accumulate-rec * 1 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(sum-rec (lambda (x) (* x 2)) 1 (lambda (x) (+ x 1)) 10)

(sum-rec (lambda (x) x) 3 (lambda (x) (+ x 1)) 10)

(product-rec (lambda (x) x) 1 (lambda (x) (+ x 1)) 5)

(product-iter (lambda (x) (+ x 1)) 1 (lambda (x) (+ x 2)) 6)


(define (filtered-accumulate combiner filter null-value term a next b)
  (cond ((> a b) null-value)
	((filter a) (combiner (term a) (filtered-accumulate combiner filter null-value term (next a) next b)))
	(else
	 (filtered-accumulate combiner filter null-value term (next a) next b))))

(define (sum-odd term a next b)
  (filtered-accumulate + (lambda (x) (not (even? x))) 0 term a next b))

(sum-odd (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)

(define (f g)
  (g 2))

(f f)

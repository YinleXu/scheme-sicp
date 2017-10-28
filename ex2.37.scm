(define (accumulate op initial sequence)
 (if (null? sequence)
     initial
     (op (car sequence)
	 (accumulate op initial (cdr sequence)))))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define vec1 '(1 1 0))

(define vec2 '(1 0 1))

(dot-product vec1 vec2)

(define (matrix-*-vector mat vec)
  (map (lambda (v)
	 (dot-product v vec))
       mat))

(define mat1 '((2 0 2) (1 1 0) (1 1 1)))
(define mat2 '((2 2 2) (2 0 2) (4 0 0)))

(matrix-*-vector mat1 vec1)
(matrix-*-vector mat2 vec2)

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose mat1)
(transpose mat2)

(define (matrix-*-matrix m n)
  (let ((n-t (transpose n)))
    (map (lambda (v)
	   (matrix-*-vector n-t v))
	 m)))

(matrix-*-matrix mat1 mat1)
(matrix-*-matrix mat1 mat2)

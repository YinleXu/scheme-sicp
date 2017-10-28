;;exercise 2.2

(define (point-make x y)
  (cons x y))

(define (point-x p)
  (car p))

(define (point-y p)
  (cdr p))

(define (segment-make start end)
  (cons start end))

(define (segment-start seg)
  (car seg))

(define (segment-end seg)
  (cdr seg))

(define (display-point p)
  (newline)
  (display "(")
  (display (point-x p))
  (display ",")
  (display (point-y p))
  (display ")"))

(define (segment-midpoint seg)
  (let ((x-start (point-x (segment-start seg)))
	(y-start (point-y (segment-start seg)))
	(x-end (point-x (segment-end seg)))
	(y-end (point-y (segment-end seg))))
    (point-make (/ (+ x-start x-end) 2)
		(/ (+ y-start y-end) 2))))

(define line1 (segment-make (point-make 4 -3)
			    (point-make 2  7)))

(display-point (segment-midpoint line1))

;;exercise 2.3

; very useful interface for line segments that
; calculates the length of the lines
(define (segment-length seg)
  (let ((x-start (point-x (segment-start seg)))
	(y-start (point-y (segment-start seg)))
	(x-end (point-x (segment-end seg)))
	(y-end (point-y (segment-end seg))))
    (sqrt (+ (square (- x-start x-end))
	     (square (- y-start y-end))))))


(define (segment-slope seg)
  (let ((x-start (point-x (segment-start seg)))
	(y-start (point-y (segment-start seg)))
	(x-end (point-x (segment-end seg)))
	(y-end (point-y (segment-end seg))))
    (/ (- y-end y-start)
       (- x-end x-start))))

(segment-slope line1)

(define (rectangle-make length width)
  (cons length width))

(define (rectangle-length rect)
  (car rect))

(define (rectangle-width rect)
  (cdr rect))

(define (rectangle-perimeter rect)
  (let ((seg-l (rectangle-length rect))
	(seg-w (rectangle-width rect)))
    (* 2 (+ (segment-length seg-l)
	    (segment-width seg-w)))))

(define (rectangle-area rect)
  (let ((seg-l (rectangle-length rect))
	(seg-w (rectangle-width rect)))

; [sets as ordered lists]

; preamble
(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(element-of-set? 4 '(1 2 4 5))
(element-of-set? 1 '(2 3))
(element-of-set? 3 '(1 2 4))
(element-of-set? 6 '(1 2 3))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((= (car set1) (car set2))
	 (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
	((< (car set1) (car set2))
	 (intersection-set (cdr set1) set2))
	(else (intersection-set set1 (cdr set2)))))

(intersection-set '(1 2 4 5 7) '(2 3 5 6 9))
(intersection-set '(1 2 4 6 7) '(1 3 5 7 9 10))

;ex 2.61
(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
	((< x (car set)) (cons x set))
	((= x (car set)) set)
	(else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 4 '(1 2 3))
(adjoin-set 2 '(1 2 3))
(adjoin-set 1 '(2 3 4))
(adjoin-set 3 '(1 2 4))

; ex 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((= (car set1) (car set2))
	 (cons (car set1) (union-set (cdr set1) (cdr set2))))
	((< (car set1) (car set2))
	 (cons (car set1) (union-set (cdr set1) set2)))
	(else (cons (car set2) (union-set set1 (cdr set2))))))

(union-set '(1 2 3) '(4 6))
(union-set '(4 6) '(1 2 3))
(union-set '(1 2 3 4) '(2 3))
(union-set '(1 2 3) '(3 4 5))



; project Euler, problem 1
(define (multiple-3-5 start limit)
  (cond ((>= start limit) 0)
	((or (= (remainder start 3) 0) (= (remainder start 5) 0))
	 (+ start (multiple-3-5 (+ start 1) limit)))
	(else (multiple-3-5 (+ start 1) limit))))
(multiple-3-5 1 1000)

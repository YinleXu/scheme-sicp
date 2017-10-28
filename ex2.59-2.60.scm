; sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(element-of-set? 4 '(1 2 4 3))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(adjoin-set 2 '(1 3 4))
(adjoin-set 3 '(1 2 3))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(intersection-set '(1 2 3 4) '(2 5))
(intersection-set '(1 2 3) '(5 3 1 2))
(Intersection-set '(1 3 5) '(2 4 6))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (cons (car set1) (union-set (cdr set1) set2)))))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))

(union-set '(1 2 3 4) '(4 2 0 5 1))

; ex 2.60
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(adjoin-set 4 '(1 2 3 4))

(define (union-set set1 set2)
  (append set1 set2))

(union-set '(1 2 4) '(2 5 6))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

(intersection-set '(1 2 2 3 4) '(3 2))

; the set expands invariably with every 'adjoin-set' and 'union-set' operation
; while the running-time of element-of-set? is linearly proportional to the size
; of the set, to which the running time of 'intersection-set' procedure is
; linearly dependent. On the other hand, the running time of 'adjoin-set' is
; only Theta(1) and 'union-set' proportional to size of 'set1' only. Therefore, ; this set representation with duplicates allowed is good for the applications
; where 'adjoin-set' and 'union-set' is majorly used while 'intersection-set'
; and 'element-of-set' is less used.

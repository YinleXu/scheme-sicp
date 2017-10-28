; [sets as binary trees]

; preamble
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set)) (element-of-set? x (left-branch set)))
	(else (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	(else
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))


; ex 2.63
(define tree2 (make-tree 3
			 (make-tree 1 '() '())
			 (make-tree 7
				    (make-tree 5 '() '())
				    (make-tree 9 '() (make-tree 11 '() '())))))

(Define tree1 (make-tree 7
			 (make-tree 3
				    (make-tree 1 '() '())
				    (make-tree 5 '() '()))
			 (make-tree 9
				    '()
				    (make-tree 11 '() '()))))

(define tree3 (make-tree 5
			 (make-tree 3
				    (make-tree 1 '() '())
				    '())
			 (make-tree 9
				    (make-tree 7 '() '())
				    (make-tree 11 '() '()))))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(tree->list-1 tree1)
(tree->list-1 tree2)
(tree->list-1 tree3)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree) 
			    (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(tree->list-2 tree1)
(tree->list-2 tree2)
(tree->list-2 tree3)

; a. two procedures produce the same result for every tree
; b. the first procedure creates a 'sub-list' for each branch of the tree and uses
; the procedure 'append' to merge the two. Therefore, each element of the final
; list has been 'cons'ed in Theta(logN) 'sub-lists', in total then there is 
; Theta(NlogN) 'cons' procedure calls.
;    the second procedure 'cons' constructs the final list one element by another
; without using 'sub-list' as intermediate, therefore 'cons' procedure is only 
; invoked Theta(N) 'cons' times
; therefore, 'tree->list-2' grows more slowly 

; ex 2.64

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- (- n 1) left-size)))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts) right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(list->tree '(1 3 5 7 9 13 17 19))

; 'make-tree' procedure is invoked once for every entry through out the recursive
; process, inplying a Theta(N) time complexity

; ex 2.65
(define (union-set-list set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((= (car set1) (car set2))
	 (cons (car set1) (union-set-list (cdr set1) (cdr set2))))
	((< (car set1) (car set2))
	 (cons (car set1) (union-set-list (cdr set1) set2)))
	(else (cons (car set2) (union-set-list set1 (cdr set2))))))

(define (intersection-set-list set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((= (car set1) (car set2))
	 (cons (car set1) (intersection-set-list (cdr set1) (cdr set2))))
	((< (car set1) (car set2))
	 (intersection-set-list (cdr set1) set2))
	(else (intersection-set-list set1 (cdr set2)))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-list
	       (tree->list-2 set1)
	       (tree->list-2 set2))))

(intersection-set (list->tree '(1 2 4 6 7))
		  (list->tree '(1 3 5 7 9 10)))

(define (union-set set1 set2)
  (list->tree (union-set-list (tree->list-2 set1)
			      (tree->list-2 set2))))

(union-set (list->tree '(1 3 5 6 7))
	   (list->tree '(2 3 5 7 9)))


; irrelevant
(define (sqrt1 x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average a b)
    (/ (+ a b) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (sqrt-iter 1.0))

(sqrt1 4.1)
(sqrt 4.1)
 

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (define (weigh-branch branch)
    (let ((structure (branch-structure branch)))
      (if (pair? structure)
	  (total-weight structure)
	  structure)))
  (+ (weigh-branch (left-branch mobile))
     (weigh-branch (right-branch mobile))))

(define branch1 (make-branch 2 3))

(define branch2 (make-branch 3 2))

(define branch3 (make-branch 4 5))

(define mobile1 (make-mobile branch1 branch2))

(define branch4 (make-branch 5 mobile1))

(define mobile2 (make-mobile branch3 branch4))

(define branch5 (make-branch 4 mobile1))

(define mobile3 (make-mobile branch3 branch5))

(total-weight mobile1)

(total-weight mobile2)

(define (balanced? mobile)
  (define (branch-balanced? branch)
    (let ((structure (branch-structure branch)))
      (if (pair? structure)
	  (balanced? structure)
	  true)))
  (define (branch-weight branch)
    (let ((structure (branch-structure branch)))
      (if (pair? structure)
	  (total-weight structure)
	  structure)))
  (let ((left-length (branch-length (left-branch mobile)))
	(right-length (branch-length (right-branch mobile)))
	(left-weight (branch-weight (left-branch mobile)))
	(right-weight (branch-weight (right-branch mobile))))
    (and (= (* left-length left-weight) (* right-length right-weight))
	(branch-balanced? (left-branch mobile))
	(branch-balanced? (right-branch mobile)))))

(balanced? mobile1)

(balanced? mobile2)

(balanced? mobile3)

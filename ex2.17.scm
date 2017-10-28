(define (last-pair ls)
  (if (null? (cdr ls))
      (car ls)
      (last-pair (cdr ls))))

(define list-a (list 1 3 5 2 4 6))

(last-pair list-a)

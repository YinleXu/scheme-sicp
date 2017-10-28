(define (or-dummy a b) (or a b))

(define (exist? seq element)
  (accumulate or-dummy false (map (lambda (x-in-seq) (equal? x-in-seq element))
			    seq)))

(exist? '((1 2) (1 3) (2 3) (3 3)) '(2 3))
(exist? '((1 2) (1 3) (2 3) (3 3)) '(2 4))

; eval the following "make,get" functions for the "pair" representation of the positions in a board
(define (make-position row col)
  (cons row col))
(define (position-row position)
  (car position))
(define (position-col position)
  (cdr position))

(define (queens board-size)
  (define (adjoin-position new-row k existing-board)
    (cons (make-position new-row k) existing-board))
  
  (define (safe? col-num board)
    (define (unsafe-positions new-row new-col)
      (map (lambda (num-col-down)
	     (list (make-position new-row (- new-col num-col-down))
		   (make-position (- new-row num-col-down) (- new-col num-col-down))
		   (make-position (+ new-row num-col-down) (- new-col num-col-down))))
	   (enumerate-interval 1 (- new-col 1))))
    (not (accumulate or-dummy false (map exist?
				    (unsafe-positions (position-row (car board)) col-num)
				    (cdr board)))))
;  (safe? 4 '((2 . 4) (6 . 3) (1 . 2) (3 . 1)))
;  (safe? 4 '((5 . 4) (6 . 3) (1 . 2) (3 . 1)))

  (define (queen-cols k)
    (let ((empty-board '()))
      (if (= k 0)
	  (list empty-board)
	  (filter
	   (lambda (board) (safe? k board))
	   (flatmap (lambda (existing-board)
		      (map (lambda (new-row)
			     (adjoin-position new-row k existing-board))
			   (enumerate-interval 1 board-size)))
		    (queen-cols (- k 1)))))))

  (queen-cols board-size))


(queens 4)

(queens 8)

; eval the following two expressions to use the alternative representation of each position
; this representation uses row number, ordered in column numbers, for each position in a board (the "index" of each element in the list represents the column number of that row)
(define (make-position row col) row)
(define (position-row position) position)

; question for myself: these results contain symmetric duplicates, effectively doubles the program's effort, how to solve this? 


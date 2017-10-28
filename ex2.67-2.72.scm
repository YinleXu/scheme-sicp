; preambles
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf leaf)
  (cadr leaf))

(define (weight-leaf leaf)
  (caddr leaf))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	      (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else  (cons (car set)
		     (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs) 
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair) (cadr pair))
		    (make-leaf-set (cdr pairs))))))

; ex 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

; ex 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))


(define (encode-symbol symbol tree)
  (define (element-of-set? x set)
    (cond ((null? set) false)
	  ((eq? x (car set)) true)
	  (else (element-of-set? x (cdr set)))))
 	
  (define (encode-symbol-true symbol tree)
    (cond ((leaf? tree) '())
	  ((element-of-set? symbol (left-branch tree))
	   (cons 0 (encode-symbol-true symbol (left-branch tree))))
	  (else
	   (cons 1 (encode-symbol-true symbol (right-branch tree))))))

  (if (element-of-set? symbol (symbols tree))
      (encode-symbol-true symbol tree)
      (error "symbol non-exist -- ENCODE-SYMBOL" symbol)))

(encode '(a d a b b c a) sample-tree)

; ex 2.69
; the leaf-set given has to be in descending order
; the question could be way more easier if the leaf-set is in ascending order
(define (generate-huffman-tree pairs)
; make-leaf-set procedure here creates the descending ordered leaf-set
  (define (adjoin-set x set)
    (cond ((null? set) (list x))
	  ((> (weight x) (weight (car set))) (cons x set))
	  (else  (cons (car set)
		       (adjoin-set x (cdr set))))))

  (define (make-leaf-set pairs)
    (if (null? pairs) 
	'()
	(let ((pair (car pairs)))
	  (adjoin-set (make-leaf (car pair) (cadr pair))
		      (make-leaf-set (cdr pairs))))))

  (successive-merge (make-leaf-set pairs)))

(define (successive-merge  leaf-set)
  (define (adjoin-leaf original-set new-branch)
    (cond ((null? original-set) (cons new-branch '()))
	  ((> (weight new-branch) (weight (car original-set)))
	   (cons new-branch original-set))
	  (else (cons (car original-set)
		      (adjoin-leaf (cdr original-set) new-branch)))))
  
  (define (split-set leaf-set-left leading-set)
    (cond ((null? leaf-set-left) (error "empty leaf set given!"))
	  ((null? (cddr leaf-set-left))
	   (cons leading-set
		 (make-code-tree (car leaf-set-left) (cadr leaf-set-left))))
	  (else (split-set (cdr leaf-set-left) 
			   (append leading-set (list (car leaf-set-left)))))))

  (if (null? (cdr leaf-set))
      (if (leaf? (car leaf-set)) 
	  (list (car leaf-set) '() (symbols (car leaf-set)) 
		(weight (car leaf-set)))
	  leaf-set)	  
      (let ((split (split-set leaf-set '())))
	(successive-merge (adjoin-leaf (car split) (cdr split))))))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))

; this is the 'proper' way for implementing generate-huffman-tree with
; the given leaf-set in ascending order 
(define (generate-huffman-tree pairs)
  (define (successive-merge leaf-set)
    (

  (successive-merge (make-leaf-set pairs)))

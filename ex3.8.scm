(define f
  (let ((state -1))
    (lambda (x)
      (set! state (+ state 1))
      (if (= state 0)
	  (if (= x 0)
	      0
	      1)
	  0))))

(+ (f 0) (f 1))

(+ (f 1) (f 0))

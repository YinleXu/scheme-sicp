(define (make-monitored f)
  (let ((counter 0))
    (lambda (command)
      (cond ((eq? command 'how-many-calls?) counter)
	    ((eq? command 'reset-count) 
	     (begin 
	       (set! counter 0)
	       counter))
	    (else
	     (begin 
	       (set! counter (+ counter 1))
	       (f command)))))))

(define mt-sqrt (make-monitored sqrt))

(mt-sqrt 'how-many-calls?)
(mt-sqrt 100)
(mt-sqrt 'how-many-calls?)
(mt-sqrt 256)
(mt-sqrt 'how-many-calls?)
(mt-sqrt 'reset-count)

(mt-sqrt 'how-many-calls?)
(mt-sqrt 369)
(mt-sqrt 'how-many-calls?)
(mt-sqrt 169)
(mt-sqrt 'how-many-calls?)
(mt-sqrt 'reset-count)

(define (make-account balance password)
  (define (withdraw amount)
    (if (<= amount balance)
	(begin
	  (set! balance (- balance amount))
	  balance)
	"Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (call-the-cops dummy)
    "cops en route!!")

  (let ((incorrect-time 0))
    (lambda (pswd-in m)
      (if (eq? pswd-in password)
	  (begin
	    (set! incorrect-time 0)
	    (cond ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else
		   (error "unknown request -- MAKE-ACCOUNT" m))))
	  (if (= incorrect-time 6)
	      call-the-cops
	      (begin
		(set! incorrect-time (+ incorrect-time 1))
		(lambda (dummy) "incorrect password!")))))))

(define A1 (make-account 100 'hellow))

((A1 'hellow 'withdraw) 50)
((A1 'hellow 'deposit) 1000)
((A1 'hellow 'withdraw) 5000)

((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hellow 'withdraw) 2000)

((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hellow 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)
((A1 'hello 'withdraw) 100)

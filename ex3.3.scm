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
  
  (lambda (pswd-in m)
    (if (eq? pswd-in password)
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else
	       (error "unknown request -- MAKE-ACCOUNT" m)))
	(lambda (dummy) "incorrect password!"))))

(define A1 (make-account 100 'hellow))

((A1 'hellow 'withdraw) 50)
((A1 'hellow 'deposit) 1000)
((A1 'hellow 'withdraw) 5000)

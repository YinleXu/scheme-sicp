((define (make-account balance password)
  (define (withdraw amount)
    (if (<= amount balance)
	(begin
	  (set! balance (- balance amount))
	  balance)
	"Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else
	   (error "unknown request -- MAKE-ACCOUNT" m))))
  
  (password-protect password dispatch))

(define (password-protect password object)
  (define (call-the-cops)
    "cops en route!!")

  (let ((fail-time 0))
    (lambda (pw-in msg-in)
      (if (eq? password pw-in)
	  (object msg-in)
	  (if (= fail-time 6)
	      (call-the-cops)
	      (begin
		(set! fail-time (+ fail-time 1))
		(lambda (dummy) "wrong password!")))))))

(define A (make-account 100 'apassword))

((A 'apassword 'withdraw) 30)

(define (make-joint account old-pw new-pw)
  (define (same-dispatch msg)
    (account old-pw msg))

  (password-protect new-pw same-dispatch))

(define B (make-joint A 'apassword 'bpassword))

((B 'apassword 'withdraw) 10)

((B 'bpassword 'withdraw) 20)

((A 'bpassword 'withdraw) 10)

((A 'apassword 'withdraw) 10)

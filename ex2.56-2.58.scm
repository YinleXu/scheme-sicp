; prerequisite
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define( =number? expr num)
  (and (number? expr) (= expr num)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))

(define (make-product m1 m2) 
  (cond ((=number? m1 1) m2)
	((=number? m2 1) m1)
	((or (=number? m1 0) (=number? m2 0)) 0)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (deriv expr var)
  (cond ((number? expr) 0)
	((variable? expr)
	 (if (same-variable? expr var) 1 0))
	((sum? expr)
	 (make-sum (deriv (addend expr) var)
		   (deriv (augend expr) var)))
	((product? expr)
	 (make-sum
	  (make-product (multiplier expr)
			(deriv (multiplicand expr) var))
	  (make-product (deriv (multiplier expr) var)
			(multiplicand expr))))
	((exponentiation? expr)
	 (make-product (exponent expr)
		       (make-product
			(make-exponentiation (base expr)
					     (make-sum (exponent expr) -1))
			(deriv (base expr) var))))
	(else
	 (error "unknown expression type -- DERIV" expr))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv 1 'x)

; ex 2.56
(define (exponentiation? x)
  (eq? (car x) '**))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation b e)
  (cond ((or (=number? e 0) (=number? b 1)) 1)
	((=number? b 0) 0)
	((=number? e 1) b)
	((and (number? b) (number? e)) (expt b e))
	(else (list '** b e))))

(deriv '(** (+ x 3) 2) 'x)

; ex 2.57
(define (augend s)
  (if (pair? (cdddr s))
      (cons '+ (cddr s))
      (caddr s)))

(define (multiplicand p)
  (if (pair? (cdddr p))
      (cons '* (cddr p))
      (caddr p)))

(deriv '(* x y (+ (** x 3) x 3)) 'x)

; ex 2.58a
(define (make-sum addend augend)
  (cond ((=number? addend 0) augend)
	((=number? augend 0) addend)
	((and (number? addend) (number? augend)) (+ addend augend))
	(else  (list addend '+ augend))))

(define (make-product multiplier multiplicand)
  (cond ((=number? multiplier 1) multiplicand)
	((=number? multiplicand 1) multiplier)
	((and (number? multiplier) (number? multiplicand))
	 (* multiplier multiplicand))
	((or (=number? multiplier 0) (=number? multiplicand 0)) 0)
	(else (list multiplier '* multiplicand))))

(define (sum? expr)
  (and (pair? expr) (eq? (cadr expr) '+)))

(define (product? expr)
  (and (pair? expr) (eq? (cadr expr) '*)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(deriv '(1) 'x)
(deriv '(x + 3) 'x)

(deriv '(x * y) 'x)

(deriv '((x + 3) * (y * x)) 'x)

; ex 2.58b
(define (sum? expr)
  (and (pair? expr)
       (pair? (cdr expr))
       (or (eq? (cadr expr) '+) (sum? (cddr expr)))))

(sum? '(1 * a * c * 2))

(define (addend s)
  (define (addend-list x)
    (if (eq? (cadr x) '+)
	(cons (car x) '())
	(cons (car x) (cons (cadr x) (addend-list (cddr x))))))
  (let ((flawed-addend (addend-list s)))
    (if (pair? (cdr flawed-addend))
	flawed-addend
	(car flawed-addend))))

(addend '(1 + 2 + 3))
(addend '(1 * 2 * 3 + 4))

(define (augend s)
  (if (eq? (cadr s) '+)
      (if (pair? (cdddr s))
	  (cddr s)
	  (caddr s))
      (augend (cddr s))))

(augend '(1 + 2 + 3))
(augend '(1 * 2 * 3 + 4))

(define (product? expr)
  (and (pair? expr)
       (eq? (cadr expr) '*)
       (or (not (pair? (cdddr expr)))
	   (not (sum? (cddr expr))))))

; (product? '(1 * 2 * 3 + 4))
; (product? '(1 * 2 * 3 * 4))
; (product? '(1 * 2 + 3 * 4))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (if (pair? (cdddr p))
      (cddr p)
      (caddr p)))

(multiplier '(1 * 2 * 3 *4))
(multiplicand '(1 * 2 * 3 * 4))

(deriv '(x + 3 * (x + y + 2) * 2 + 4) 'x)

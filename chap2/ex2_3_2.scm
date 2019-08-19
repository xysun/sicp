; symbolic differentiation
(define (variable? e)
  (symbol? e)
)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (=number? x v)
  (and (number? x) (= x v))
)

(define (make-sum v1 v2)
  (cond ((=number? v1 0) v2)
    ((=number? v2 0) v1)
    ((and (number? v1) (number? v2)) (+ v1 v2))
    (else (list '+ v1 v2))
  )
)

(define (make-product v1 v2)
  (cond ((or (=number? v1 0) (=number? v2 0)) 0)
    ((=number? v1 1) v2)
    ((=number? v2 1) v1)
    ((and (number? v1) (number? v2)) (* v1 v2))
    (else (list '* v1 v2))
  )
)

(define (sum? x)
  (and (pair? x) (eq? '+ (car x)))
)

(define (addend e)
  (cadr e)
)

;2.57
(define (augend e)
  (if (null? (cdddr e)) ; only 2 arguments
    (caddr e)
    (make-sum (addend (cdr e)) (augend (cdr e))))
)

(define (multiplier e)
  (cadr e)
)

;2.57
(define (multiplicand e)
  (if (null? (cdddr e))
    (caddr e)
    (make-product (multiplier (cdr e)) (multiplicand (cdr e)))
  )
)

(define (product? x)
  (and (pair? x) (eq? '* (car x)))
)

(define (deriv exp var)
  (cond ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var)
      1
      0))
    ((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
    ((product? exp) (make-sum 
      (make-product (multiplier exp) (deriv (multiplicand exp) var))
      (make-product (multiplicand exp) (deriv (multiplier exp) var))
    ))
    ((exponentiation? exp)
      (make-product 
        (deriv (base exp) var)
        (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1))))
    )
    (else (error "unknown expression type" exp))
  )
)

; 2.56
(define (exponentiation? e)
  (and (pair? e) (= '** (car e)))
)

(define (base e)
  (cadr e)
)

(define (exponent e)
  (caddr e)
)

(define (make-exponentiation base exp)
  (cond ((= exp 0) 1)
    ((= exp 1) base)
    ((= base 1) 1)
    (else (list '** base exp))
  )
)

; 2.59

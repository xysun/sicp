; exercise for 1.3.1

(define (h a b n)
  (/ (- b a) n))


(define (multiplier curr)
  (cond 
    ((= curr 0) 1)
    ((even? curr) 2)
    (else 4)
  )
)

(define (simpsons-aux f a b n curr h)
  (if (> curr n)
    0
    (
      +
      (* (multiplier curr) (f (+ a (* curr h))))
      (simpsons-aux f a b n (+ 1 curr) h)
    )
    )
)

(define (simpsons f a b n)
  (
    *
    (/ (h a b n) 3)
    (simpsons-aux f a b n 0 (h a b n))  
    )
)

(define (cube x)
  (* x x x))

; 1.30
; original
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))
    )
)

(define (ident x)
  x)

(define (incr x)
  (+ 1 x ))

; tail recursive
(define (sum-tr term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a))))
  )
  (iter a 0)
)
; 1.31
(define (product a term next b)
  (if (> a b)
    1
    (* (term a) (product (next a) term next b)))
)

(define (product-tr a term next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))

  (iter a 1)
)

(define (factorial n)
  (product-tr 1 ident incr n))

; PIE
; numerator: 2,4,4,6,6,8,8 
; denominator: 3,3,5,5,7,7

(define (numerator i)
(cond ((= i 0) 2)
  ((even? i) (+ i 2))
  (else (+ i 3)))
)

(define (denominator i)
(if (even? i)
  (+ i 3)
  (+ i 2))
)

(define (pi-term i)
  (/ (numerator i) (denominator i)))

(define (pi max)

  (exact->inexact 
    (* 4
      (product-tr 0 pi-term incr max))  
    )
)

; 1.32
(define (accumulate combiner null-value term a next b)
(if (> a b)
  null-value
  (
    combiner
    (term a)
    (accumulate combiner null-value term (next a) next b)
  ))
)

(define (accumulate-tr combiner null-value term a next b)

  (define (iter a result)
  (if (> a b)
    result
    (iter (next a) (combiner result (term a)))
    )
  )
(iter a null-value)

)

; 1.33
(define (filter-accumulate combiner null-value term a next b filter)

(if (> a b)
  null-value
  
  (if (filter a)
    (
      combiner (term a) (filter-accumulate combiner null-value term (next a) next b filter)
    )
    (filter-accumulate combiner null-value term (next a) next b filter)
    )
  )
)

; 1.33 b

(define (gcd a b)
(if (= b 0)
  a
  (gcd b (remainder a b)))
)

(define (relative-prime i n)
  (= 1 (gcd i n)))

(define (product-of-relative-primes n)

  (define (filter i)
    (relative-prime i n))

  (filter-accumulate * 1 ident 1 incr n filter)

)
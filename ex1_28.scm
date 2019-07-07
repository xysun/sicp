(define (square x)
  (* x x))

; return 0 if x**2 % m = 1 for nontrivial x (x != 1 and x != m-1); else return x**2
(define (sgmod x m)
  (let ((squared (square x)))
    (cond (
      
      (and (= 1 (remainder squared m))
        (not (= x 1))
        (not (= x (- m 1)))
      )
      0)
      (else squared))
  )
)

(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
          (remainder (sgmod (expmod base (/ exp 2) m) m) 
                    m)) 
        (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

(define (feynman-test n) ; for random a < n, a ** n % n = a
  (define (try-it a)
  (= 1 (expmod a (- n 1) n)))    

    (try-it (+ 1 (random (- n 1)))) ; random returns nonnegative < 
)

(define (fast-prime? n times)
(cond ((= times 0) true)
  ((feynman-test n) (fast-prime? n (- times 1)))
  (else false)
  )  
)

; cannot fool this carmichael anymore
(define (carmichael n start)
  (cond ((= start n) true)
    ((= 1 (expmod start (- n 1) n)) (carmichael n (+ 1 start)))
    (else false)
    ))
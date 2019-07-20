; covers 1.22 to 1.24

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

; 1.23
(define (next n)
  (if (= n 2) ; extra if check, hence runtime not halved
    3
    (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))
  )
)

(define (divides? a b)
  (= (remainder b a) 0)
)

(define (prime? n)
  (= (smallest-divisor n) n)
)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)) 
)

(define (start-prime-test n start-time)
    (if (prime? n)
      (report-time (- (runtime) start-time))
    )
)

(define (report-time elapsed)
    (display " *** ")
    (display elapsed)
)

(define (search-for-primes from total)

  (if (= total 0)
    (display "done")
    (if (prime? from)
      (begin 
        (timed-prime-test from)
        (search-for-primes (+ from 2) (- total 1))
        )
      (search-for-primes (+ from 2) total))
    ))

; 1.24

; slow version

; (define (expo base exp)
;   (cond ((= exp 0) 1)
;     ((even? exp) (square (expo base (/ exp 2))))
;     (else (* base (expo base (- exp 1))))
;     ))

; (define (expmod base exp m)
;   (remainder (expo base exp) m))

; fast version: because it never calculates a truly gigantic exponential, always after `remainder`
(define (expmod base exp m) 
  (cond ((= exp 0) 1) 
        ((even? exp) 
          (remainder (square (expmod base (/ exp 2) m)) 
                    m)) 
        (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

(define (feynman-test n) ; for random a < n, a ** n % n = a
  (define (try-it a)
  (= a (expmod a n n)))    

    (try-it (+ 1 (random (- n 1)))) ; random returns nonnegative < 
)

(define (fast-prime? n times)
(cond ((= times 0) true)
  ((feynman-test n) (fast-prime? n (- times 1)))
  (else false)
  )  
)

(define (start-fast-prime n start-time)
  (if (fast-prime? n 10)
    (report-time (- (runtime) start-time))
    )  
)

(define (timed-fast-prime n)
      (display n)
      (start-fast-prime n (runtime))
)

(define (carmichael n start)
  (cond ((= start n) true)
    ((= start (expmod start n n)) (carmichael n (+ 1 start)))
    (else false)
    ))
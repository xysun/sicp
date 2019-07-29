; 2.7
(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

; 2.8
(define (sub-interval i1 i2)
  (make-interval (- (lower-bound i1) (upper-bound i2)) (- (upper-bound i2) (lower-bound i1)))
)

; 2.10 use (error "err msg") keyword
(define (span-zero interval)
  (and (<= (lower-bound interval) 0) (>= (upper-bound interval) 0))
)

; 2.11
; 0: --; 1: -+; 2: ++
(define (interval-type i)
  (if (< (lower-bound i) 0)
    (if (< (upper-bound i) 0)
      0
      1)
    2)
)

(define (mul-interval x y)
(let ((a (interval-type x)) (b (interval-type y)))
  (cond 
    ((= a 0) 
     (cond 
       ((= b 0) (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
       ((= b 1) (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
       ((= b 2) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
     )
    )
    ((= a 1) 
     (cond 
       ((= b 0) (make-interval (* (lower-bound y) (upper-bound x)) (* (lower-bound y) (lower-bound x))))
       ((= b 1) 
         (let (
           (p1 (* (lower-bound x) (lower-bound y))) ; positive
           (p2 (* (upper-bound x) (upper-bound y))) ; positive
           (p3 (* (lower-bound x) (upper-bound y))) ; negative
           (p4 (* (upper-bound x) (lower-bound y))) ; negative
         )
         (make-interval (min p3 p4) (max p1 p2))))
       ((= b 2) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
     )
    )
    ((= a 2) 
     (cond 
       ((= b 0) (make-interval (* (lower-bound y) (upper-bound x)) (* (upper-bound y) (lower-bound x))))
       ((= b 1) (make-interval (* (lower-bound y) (upper-bound x)) (* (upper-bound y) (upper-bound x))))
       ((= b 2) (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
     )
    )
)
))

; 2.12
(define (make-center-percent c p)
  (make-interval (- c (abs (* c p))) (+ c (abs (* c p))))
)

(define (cetner  i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)
)

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2)
)

(define (percent interval)
  (/ (width interval) (center interval))
)
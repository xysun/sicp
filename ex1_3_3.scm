; exercise for section 1.3.3
; 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )
  (define (try guess)
    (let ((next (f guess)))
    (begin 
      (newline)
      (display next)
      (if (close-enough? next guess)
      next
      (try next))
      )
      )
  )

  (try first-guess)

)

(define (fixed-point-average-dampening f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )

  (define (avg v1 v2)
    (/ (+ v1 v2) 2)
  )

  (define (try guess)
    (let ((next (f guess)))
    (begin 
      (newline)
      (display next)
      (if (close-enough? next guess)
      next
      (try (avg next guess)))
      )
      )
  )

  (try first-guess)

)

(define (golden-ratio-f x)
  (+ 1 (/ 1 x))
)

(define (f2 x)
  (/ (log 1000) (log x))
)

; 1.37
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) 
        (+ (d i) (iter (+ 1 i)))
        )
      )
  )

  (iter 1)

)

; there is no iterative solution because you always depend on all values
; 1.38
(define (d i)
(if (= 0 (remainder (- i 2) 3))
  (* 2 (+ 1 (/ (- i 2) 3)))
  1)
)

(define (euler k)
  (+ 2 (cont-frac (lambda (i) 1.0) d k))
)

; 1.39
(define (tan-cf x k)
  (define (d i)
    (- (* 2 i) 1)
  )

  (define (n i)
    (if (= i 1)
      x
      (* -1 (square x)))
  )

  (cont-frac n d k)

)
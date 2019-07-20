(define (sqrt-iter prev-guess current-guess x)
  (if (good-enough? prev-guess current-guess)
    current-guess
    (sqrt-iter current-guess (improve current-guess x) x)
  )
)

(define (improve guess x)
  (average guess (/ x guess))
)

(define (average x y)
  (/ (+ x y) 2)
)

(define (good-enough? prev-guess current-guess)
; abs(1-prev/current) < 0.001
  (
    <
    (
      abs 
      (
        - 
        1
        (
          / 
          prev-guess
          current-guess
        )
      )
    )
    0.001
  )
)

(define (sqrt x)
  (sqrt-iter 100.0 1.0 x)
)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
  (else else-clause))
)
(define tolerance 0.00001)

(define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
)

; find x = f(x)
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      
        (if (close-enough? next guess)
          next
          (try next))
      )
  )

  (try first-guess)
)

(define (average v1 v2)
(/ (+ v1 v2) 2.0)
)

(define (average-damp f)
(lambda (y) (average y (f y)))
)

(define dx 0.00001)

(define (deriv g)
(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)
)
)

(define (cube x)
  (* x x x))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)

; x - g(x)/Dg(x)
(define (newton-transform g)
(lambda (x) (- x (/ (g x) ((deriv g) x)))
))


; find x such that g(x) = 0
; fixed point of x - g(x)/Dg(x)
(define (newton-method g guess)
(fixed-point (newton-transform g) guess)
)

(define (sqrt2 x)
(newton-method (lambda (y) (- (square y) x)) 1.0)
)

;1.40
(define (cubic a b c)
  (lambda (x) (+ c (* b x) (* a x x) (* x x x)))
)

;1.41
(define (double g)
  (lambda (x) (g (g x)))
)

(define (inc x)
(+ x 1)
)

;1.42
(define (compose f g)
  (lambda (x) (f (g x)))
)

;1.43
(define (repeated f n)
(if (= 1 n)
  f
  (repeated (compose f f) (- n 1)))
)

;1.44
(define (smooth f)
(define dx 0.001)
(lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3))
)

(define (n-fold-smooth f n)
(repeated (smooth f) n)
)

;1.45
(define (log2 n)
  (/ (log n) (log 2))
)

(define (nth-root x n)
  ; important: lambda goes in the end
  (fixed-point 
    ((repeated average-damp (floor (log2 n))) (lambda (y) (/ x (expt y (- n 1)))))
  1.0
  )
)

;1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess) 
    (if (good-enough? guess)
      guess
      ((iterative-improve good-enough? improve) (improve guess)))
  )
)

(define (sqrt3 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001)
  )

  (define (improve guess)
    (average guess (/ x guess))
  )

  ((iterative-improve good-enough? improve) x)

)

(define (fixed-point-2 f first-guess)
  ((iterative-improve 
    (lambda (x) (close-enough? x (f x)))
    f
  ) first-guess)
)
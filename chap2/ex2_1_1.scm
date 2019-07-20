; section 2.1.1 to 2.1.3
; 2.1
(define (gcd a b)
(if (= b 0)
  a
  (gcd b (remainder a b)))
)

(define (make-rat n d)
  (let ((g (gcd n d)) (x (* n d)))
    (if (> x 0)
     (cons (/ n g) (/ d g))
     (cons (* -1 (abs (/ n g))) (abs (/ d g)))
    )
  )  
)

; 2.2
(define (make-line starting-point end-point)
  (cons starting-point end-point)
)

(define (start-point line)
  (car line)
)

(define (end-point line)
  (cdr line)
)

(define (make-point x y)
  (cons x y)
)

(define (x-point p)
  (car p)
)

(define (y-point p)
  (cdr p)
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
)

(define (midpoint-line line)
  (let ((p1 (start-point line)) (p2 (end-point line)))
    (make-point
      (/ (+ (x-point p1) (x-point p2)) 2)
      (/ (+ (y-point p1) (y-point p2)) 2)
    )
  )
)

; 2.3
(define (point-distance p1 p2)
  (sqrt (+ 
    (square (- (x-point p1) (x-point p2)))
    (square (- (y-point p1) (y-point p2)))
  ))
)

(define (line-length line)
  (point-distance (start-point line) (end-point line))
)

; each representation needs to define get-width and get-height
; represent by width, height, top left point
; (define (make-rectangle width height top-left-point)
;   (cons top-left-point (cons width height))
; )

; (define (get-width rectangle)
;   (car (cdr rectangle))
; )

; (define (get-height rectangle)
;   (cdr (cdr rectangle))
; )

; represent by 2 adjacent lines
(define (make-rectangle line1 lin2)
  (cons line1 line2)
)

(define (get-width rectangle)
  (line-length (car rectangle))
)

(define (get-height rectangle)
  (line-length (cdr rectangle))
)

; unchanged below regardless of rectangle representation
(define (perimeter rectangle)
  (* 2 (+ (get-width rectangle) (get-height rectangle)))
)

(define (area rectangle)
  (* (get-width rectangle) (get-height rectangle))
)

; 2.5
(define (cons a b)
  (* (expt 2 a) (expt 3 b))
)

; divide n by x until it cannot be divisible by x, this way we obtain a
(define (keep-dividing n x a)
(if (= 0 (remainder n x))
  (keep-dividing (/ n x) x (+ 1 a))
  a)
)

(define (car n)
  (keep-dividing n 2 0)
)

(define (cdr n)
  (keep-dividing n 3 0)
)

; 2.6 church numerals ffs
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x))))
)

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (+  a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x))))
)
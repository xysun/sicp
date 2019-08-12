; 
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))
  )
)
;2.44
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller))))
  )
)
;2.45
(define (split t1 t2)
  (lambda (painter n) 
    (if (= n 0)
      painter
      (let ((smaller (split painter (- n 1))))
        (t1 painter (t2 smaller smaller))))
  )
)
; 2.46
(define (make-vect x y)
  (list x y)
)

(define (xcor-vect v)
  (car v)
)

(define (ycor-vect v)
  (cadr v)
)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)))
)

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)))
)

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v)))
)


; 2.47 pass; simple selectors
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2)
)

(define (get-origin frame)
  (car frame)
)

; 2.48
(define (make-segment start end)
  (list start end)
)

; 2.49
; define 4 corners of the frame:
(define c1 (origin frame))
(define c2 (add-vect (origin frame) (edge1 frame)))
(define c3 (add-vect c2 (edge2 frame)))
(define c4 (add-vect (origin frame) (edge2 frame)))

; a: (c1,c2),(c2,c3),(c3,c4),(c4,c1)
; b: (c1,c3), (c2, c4)
; c: 
(define m1 (scale-vect 0.5 (add-vect c1 c2)))
(define m2 (scale-vect 0.5 (add-vect c2 c3)))
(define m3 (scale-vect 0.5 (add-vect c3 c4)))
(define m4 (scale-vect 0.5 (add-vect c4 c1)))

; (m1,m2),(m2,m3), (m3,m4), (m4, m1)

; 2.50
(define (flip painter)
  (transform-painter painter 
    (make-vect 0 1)
    (make-vect 1 1)
    (make-vect 0 0)
  )
)

; counterclockwise by 180: (1,1), (1,0), (0,1)
; counterclockwise by 270: (0,1), (0,0), (1,1)

; 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (
      (let ((paint-down (transform-painter painter1 (make-vect 0 0) split-point (make-vect 1 0))) 
            (paint-up (transform-painter painter2 split-point (make-vect 0 1) (make-vect 1 0.5))) 
      )
        (
          (lambda (frame) (paint-down frame) (paint-up frame))
        ))
    ))
)


(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))) ; make sure image is still upright, i.e. 360
)

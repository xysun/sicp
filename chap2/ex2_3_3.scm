; 2.59
(define (element-of-set? e s)
  (cond ((null? s) #f)
    ((equal? e (car s)) #t)
    (else (element-of-set? e (cdr s)))
  )
)

(define (union-set set1 set2)
  (cond ((null? set1) set2)
    ((null? set2) set1)
    ((element-of-set? (car set1) set2)
      (union-set (cdr set1) set2))
    (else (cons (car set1) (union-set (cdr set1) set2)))
    )
)

(display "\n2.59\n")
(display (union-set (list 1 2) (list 2 3)))

; 2.60
(define (element-of-set2? e s)
  (cond ((null? s) #f)
    ((= e (car s)) #t)
    ((< e (car s)) #f)
    (else (element-of-set2? e (cdr 2)))
  )
)

; 2.61
(define (adjoin-set e s)
  (cond ((null? s) (list e))
    ((= e (car s)) s)
    ((< e (car s)) (cons e s))
    (else (cons (car s) (adjoin-set e (cdr s))))
  )
)

(display "\n2.61\n")
(display (adjoin-set 1 (list 2 3)))
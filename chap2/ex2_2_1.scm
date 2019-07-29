; 2.17
(define (last-pair l)
  (if (null? (cdr l))
    (car l)
    (last-pair (cdr l)))
)

; 2.18
(define (append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2)))
)

; (define (reverse l)
;   (if (or (null? l) (null? (cdr l)))
;     l
;     (append (reverse (cdr l)) (cons (car l) '())))
; )

(define (reverse l)
  (define (iter ll result)
    (if (null? ll)
      result
      (iter (cdr ll) (cons (car ll) result))
      )
  )

  (iter l '())
)

; 2.19
(define (no-more? coin-values)
  (null? coin-values)
)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else
      (+ (cc amount (cdr coin-values))
        (cc (- amount (car coin-values)) coin-values)
      )
    )
  )
)

; 2.20
(define nil '())

(define (same-parity first . rest)
  
  (define (iter first rest result)
    (if (null? rest)
      result
      (if (= (remainder first 2) (remainder (car rest) 2))
        (iter first (cdr rest) (cons (car rest) result))
        (iter first (cdr rest) result))
    )
  )

  (cons first (reverse (iter first rest nil)))
  
)

; 2.21
(define (square-list l)
  (if (null? l)
    l
    (cons (square (car l)) (square-list (cdr l))))
)

(define (map proc items)
  (if (null? items)
    items
    (cons (proc (car items)) (map proc (cdr items))))
)

(define (square-list-2 l)
  (map (lambda (x) (square x)) l)
)

; 2.23
(define (for-each proc items)
  (if (null? items)
    #t
    (begin
      (proc (car items))
      (for-each proc (cdr items))
    )
  )
)
; 2.78

(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents))
)

(define (type-tag contents)
  (if (number? contents)
    'scheme-number
    (if (pair? contents)
      (car contents)
      (error "must be a pair" contents)))
)

(define (contents datum)
  (if (number? datum)
    datum
    (if (pair? datum)
      (cdr datum)
      (error "must be a pair" datum)))
)
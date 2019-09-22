; all code related to data-directed programming
; MIT-scheme only

(define (attach-tag type-tag contents)
  (cons type-tag contents)
)

(define (type-tag datum)
  (car datum)
)

(define (contents datum)
  ; not bothering error check now
  (cdr datum)
)

(define registry (make-equal-hash-table))

(define (put op type item)
  (hash-table-set! registry (cons op type) item)
)

(define (get op type)
  (hash-table-ref registry (cons op type))
)

(define (install-rectangular-package )
  ; within the package they do not have type tags
  (define (real-part z)
    (car z)
  )

  (put 'real-part (list 'rectangular) real-part)
)

(define (install-polar-package)
  (define (magnitude z)
    (car z)
  )
  (define (angle z)
    (cadr z)
  )
  (define (real-part z)
    (* (magnitude z) (cos (angle z)))
  )

  (put 'real-part (list 'polar) real-part)
)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "no method" (list op type-tags)))
      ; (display proc)
    ))
)

(define (real-part z)
  (apply-generic 'real-part z)
)

; testing
(install-rectangular-package)
(install-polar-package)

(define a (attach-tag 'rectangular (cons 2 3)))
(display (real-part a))
(display "\ndone!\n")
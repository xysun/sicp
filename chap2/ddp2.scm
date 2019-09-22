; expand dpp.scm to have 2 levels of abstraction: number => scheme number | complex number => complex number = polar \ rectangular
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
  (define (tag x)
    (attach-tag 'rectangular x)
  )
  
  ; functions within the package they do not have type tags
  (define (real-part z)
    (car z)
  )

  (define (make-from-real x)
    (tag (cons x 0))
  )

  (put 'real-part (list 'rectangular) real-part)
  (put 'make-complex-from-real (list 'rectangular) make-from-real)
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

; scheme number package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x)
  )
  (put 'add (list 'scheme-number 'scheme-number) (lambda (x y) (tag (+ x y))))
)

(define (install-complex-number-package)
  (define (tag x)
    (attach-tag 'complex-number x)
  )

  (define (make-from-real x)
    ((get 'make-complex-from-real '(rectangular) ) x)
  )

  (define (add-complex x y)
    ; this should have 2 tags
    (tag (make-from-real (+ (real-part x) (real-part y))))
  )

  (put 'add (list 'complex-number 'complex-number) add-complex)
)

; x y must be same type; this will be handled in ddp3.scm
(define (add x y)
  (apply-generic 'add x y)
)



; testing
(install-rectangular-package)
(install-polar-package)

(define a (attach-tag 'rectangular (cons 2 3)))
(display (real-part a))
(display "\nTest 1 done!\n")

; test 2: add 2 complex
(install-scheme-number-package)
(install-complex-number-package)

(define t1 (attach-tag 'rectangular (cons 2 0)))
(define t2 (attach-tag 'rectangular (cons 3 0)))

(define c1 (attach-tag 'complex-number t1))
(define c2 (attach-tag 'complex-number t2))

(define r1 (add c1 c2))
(display r1)
(display "\nTest 2 done!\n")
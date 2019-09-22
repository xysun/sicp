; expand ddp2.scm to allow coersion, eg `add scheme-number complex-number`
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
(define coersion (make-equal-hash-table))

(define (put op type item)
  (hash-table-set! registry (cons op type) item)
)

; get should return false if it's not in hashtable
(define (get op type)
  (hash-table-ref registry (cons op type) (lambda () #f))
)

(define (put-coersion from to f)
  (hash-table-set! coersion (cons from to) f)
)

(define (get-coersion from to)
  (hash-table-ref coersion (cons from to) (lambda () #f))
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
        ; take into args type coersions
        (if (= (length args) 2)
          (let ((type1 (car type-tags)) (type2 (cadr type-tags)) (a1 (car args)) (a2 (cadr args)))
            (let ((t1->t2 (get-coersion type1 type2)) (t2->t1 (get-coersion type2 type1)))
              (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                (else (error "no coersion found" (list op type-tags)))
              )))
          (error "only support 2 args" (list op args)))
      ; (display proc)
    ))
)
)

(define (real-part z)
  (apply-generic 'real-part z)
)

; scheme number package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x)
  )

  ; coersion
  (define (scheme-number->complex-number x)
    (attach-tag 'complex-number ((get 'make-complex-from-real '(rectangular)) (contents x)))
  )

  (put 'add (list 'scheme-number 'scheme-number) (lambda (x y) (tag (+ x y))))
  (put-coersion 'scheme-number 'complex-number scheme-number->complex-number)
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

(install-scheme-number-package)
(install-complex-number-package)

; coerse scheme to complex
(define s1 (attach-tag 'scheme-number 2))
(define r1 ((get-coersion 'scheme-number 'complex-number) s1))
(display r1)
(display "\nTest 1 done!\n")

; we should be able to add a scheme number to a complex number
(define s2 (attach-tag 'complex-number (attach-tag 'rectangular (cons 2 0))))
(define r2 (add s1 s2))
(display r2) ; should be complex-number rectangular 4
(display "\nTest 2 done!\n")
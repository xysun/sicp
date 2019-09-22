; ex 2.82
; we assume each registered function takes a list of same type args
(define (func . args)
  body
)
; register with
(put 'op 'type func)

; procedure to coerce TODO

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
  (let ((proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= 2 (length args))
          (let ((type1 (car type-tags) (type2 (cadr type-tags) (a1 (car args)) (a2 (cadr args)))))
            (let ((t1->t2 (get-coercion type1 type2)) (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                (else error "no method for these types" (list op type-tags))
                )))
          (error "no method for these types" (list op type-tags))))))
)

; 2.83 
(define (raise x)
  (apply-generic 'raise x)
)

(put 'raise 'integer (lambda (x) (make-rational x 1)))
(put 'raise 'rational (lambda (x) (make-real (/ (numer x) (denom x)))))
(put 'raise 'real (lambda (x) (make-from-real-imag x 0)))
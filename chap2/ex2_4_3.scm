; this assumes `get and `put are available; in real life we would use a hash table or sth alike
(define (variable? e)
  (symbol? e)
)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (operator exp)
  (car exp)
)

(define (operands exp)
  (cdr exp)
)

(define (deriv exp var)
  (cond ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var)
      1
      0))
    (else ((get 'deriv (operator exp)) (operands exp) var))
  )  
)

(define (=number? x v)
  (and (number? x) (= x v))
)

; make-sum is needed for both sum and product
(define (make-sum v1 v2)
    (cond ((=number? v1 0) v2)
      ((=number? v2 0) v1)
      ((and (number? v1) (number? v2)) (+ v1 v2))
      (else (list '+ v1 v2))
    )
)

(define (install-sum-package)
  (define (addend operands)
    (car operands)
  )

  (define (augend operands)
    (cadr operands)
  )

  (define (sum-deriv operands var)
    (make-sum (deriv (addend operands) var) (deriv (augend operands) var))
  )

  (put 'deriv '+ sum-deriv)
)

; ex 2.74
; a. each division's file should be tagged with the division name
;    each division register a `put 'get-record 'division-name name file
(define (get-record name file)
  ((get 'get-record (division-tag file)) name file)
)
; b 
; `put 'get-salary 'division-name name file
(define (get-salary name file)
  ((get 'get-salary (division-tag file)) name file)
)
; c
; each division register `put 'has-name 'division-name name file
(define (filter f l)
  (if (null? l)
    '()
    (if (f (car l))
      (cons (car l) (filter f (cdr l)))
      (filter f (cdr l)))
  )
)

(define (find-employee-record name all-files)
  (map 
    (lambda (file) (get-record name file))
    (filter
      (lambda (file) ((get 'has-name (division-tag file)) name file))
      all-files
    )
  )
)

; d each division needs to implement: get-record; get-salary; has-name functions all with (name, file) as args

; 2.76
;                generic operator with explicit dispatch | data-directed | message-passing
; add new type:  update every operator | register every op for this type | add one new type
; add new op:    add one new op | register op for every type | update every object

; 2.27
(define (reverse l)
  (define (iter ll result)
    (if (null? ll)
      result
      (iter (cdr ll) (cons (car ll) result))
      )
  )

  (iter l '())
)

(define (deep-reverse l)
  (define (iter ll result)
    (if (null? ll)
      result
      (if (pair? (car ll))
        (iter (cdr ll) (cons (deep-reverse (car ll)) result))
        (iter (cdr ll) (cons (car ll) result))
      )
  ))

  (iter l '())
)

; 2.28

(define (append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2)))
)

(define (fringe tree)

  (define (iter l result)
  (if (null? l)
    result
    (if (pair? (car l))
      (iter (cdr l) (append result (fringe (car l))))
      (iter (cdr l) (append result (list (car l))))))
  )

  (iter tree '())
)
; recursive version
(define (fringe tree) 
   (define nil '()) 
   (if (null? tree)  
       nil 
       (let ((first (car tree))) 
         (if (not (pair? first)) 
             (cons first (fringe (cdr tree))) 
             (append (fringe first) (fringe (cdr tree))))))) 

; 2.29

(define (make-mobile left right)
  (list left right)
)

; structure is either a number of mobile
(define (make-branch length structure)
  (list length structure)
)

(define (left-branch mobile)
  (car mobile)
)

(define (right-branch mobile)
  (car (cdr mobile))
)

(define (branch-length branch)
  (car branch)
)

(define (branch-structure branch)
  (car (cdr branch))
)

(define (total-weight mobile)
  (cond ((null? mobile) 0)
    ((not (pair? mobile)) mobile) ; main trick: unify mobile and branch here
    (else (+ (total-weight (branch-structure (left-branch mobile))) (total-weight (branch-structure (right-branch mobile)))))  
  )
)

(define b1 (make-branch 1 2))
(define b2 (make-branch 0 3))
(define m1 (make-mobile b1 b2)) ; weight is 5; unbalanced
(define b3 (make-branch 0 m1))
(define m2 (make-mobile b1 b3)); weight is 7

(define (torch branch)
  (* (branch-length branch) (total-weight (branch-structure branch)))
)

(define (balanced? mobile)
  (if (not (pair? mobile))
    #t
    (and (= (torch (left-branch mobile)) (torch (right-branch mobile)))
      (balanced? (branch-structure (left-branch mobile)))
      (balanced? (branch-structure (right-branch mobile)))
    )
  )
)

; d
; just change the selectors

; 2.31
(define (tree-map proc tree)
  (cond 
    ((null? tree) '())
    ((pair? tree) (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))
    (else (proc tree))
  )
)

(define (square-tree tree)
  (tree-map square tree)
)

(define t1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
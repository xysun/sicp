; 2.33
(define nil '())

(define (accumulate op init sequence)
  (if (null? sequence)
    init
    (op (car sequence) (accumulate op init (cdr sequence)))
  )
)

(define (map1 f sequence)
  (accumulate (lambda (x y) (cons (f x) y)) nil sequence)
)

(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1)
)

(define (length1 sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence)
)

; 2.34
(define (horner-eval x coeffs)
  (accumulate (lambda (this-coeff high-terms) (+ this-coeff (* x high-terms))) 0 coeffs)
)

(display (horner-eval 2 (list 1 3 0 5 0 1)))

; 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (node) (if (pair? node)
    (count-leaves node)
    1)) t))
)
(display "\nex2.35\n")
(define t (list 1 (list 2 3)))
(display (count-leaves t))

; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
      (accumulate-n op init (map cdr seqs))
    )
  )
)
(display "\nex2.36\n")
(define t2 (list (list 1 2 3) (list 4 5 6)))
(display (accumulate-n + 0 t2))

; 2.37
(display "\n2.37\n")
(define (dot-product v w)
  (accumulate + 0 (map * v w))
)
(display (dot-product (list 1 2 3) (list 4 5 6)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m)
)
(display "\ntest\n")
(define m1 (list (list 1 2 3) (list 4 5 6)))
(define v1 (list 1 2 3))
(display (matrix-*-vector m1 v1)) ; should have 2 rows

(define (transpose m)
  (accumulate-n cons nil m)
)
(display "\ntest\n")
(display (transpose m1)) ; should have 3 rows

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m))
)
(define m2 (list (list 1 2) (list 3 4) (list 5 6))); 3x2
(display "\ntest\n")
(display (matrix-*-matrix m1 m2)); should be 2x2

; 2.38
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest)) (cdr rest))
    )
  )
  (iter init seq)
)

; 2.39
(define (reverse seq)
  (accumulate (lambda (curr rest) (append rest (list curr))) nil seq)
)

(define (reverse2 seq)
  (fold-left (lambda (rest curr) (cons curr rest)) nil seq)
)

; 2.40

(define (enumerate-interval start end)
  (define (iter result next)
    (if (> next end)
      result
      (iter (cons next result) (+ 1 next)))
  )

  (reverse (iter nil start))
)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq))
)

(define (unique-pairs n)
  (flatmap
    (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)
  )
)

; 2.41

(define (triple-pairs n)
  (
    flatmap 
    (lambda (i) (
      flatmap
      (lambda (j) (
        map
        (lambda (k) (list i j k))
        (enumerate-interval 1 (- j 1))
      ))
      (enumerate-interval 1 (- i 1))
    ))
    (enumerate-interval 1 n)
  )
)

(define (filter proc? seq)
  (if (null? seq)
    nil
    (if (proc? (car seq))
      (cons (car seq) (filter proc? (cdr seq)))
      (filter proc? (cdr seq))))
)

(define (triple-sum n s)
  (filter 
  (lambda (pair) (= s (accumulate + 0 pair)))
   (triple-pairs n)
  )
)

; 
; represent each position as a list of 2 elements, (row, col)
; represent a solution as a list of positions, eg. [(row1, col1), ..., (row8, col8)]

(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens)
)



(define (safe-pair p1 p2)
  (define (not-safe p1 p2)
  ; not on same row, same col or diagnal
  (let ((r1 (car p1)) (c1 (cadr p1)) (r2 (car p2)) (c2 (cadr p2)))
    (
      or
      (= r1 r2)
      (= c1 c2)
      (= 1 (abs (- r1 r2)) (abs (- c1 c2))) ; diagnal
    ))
  )
  (not (not-safe p1 p2))  
)

(define (all proc seq)
  (if (null? seq)
    #t
    (if (proc (car seq))
      (all proc (cdr seq))
      #f)
  )
)

(define (safe? k positions)
  ; find the position at kth column, check it against all the others
  ; (display k)
  ; (display positions)

  (if (null? (cdr positions))
    #t ; single element
    (let ((p (filter (lambda (position) (= k (cadr position))) positions)))
      (all 
        (lambda (position) (safe-pair position (car p)))
        (filter (lambda (position) (not (= k (cadr position)))) positions)
        )  
      )
  )
)

(define (queens board-size)
  (define (queen-cols k)
    ; (display k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens) (
            map 
            (lambda (new-row) (adjoin-position new-row k rest-of-queens))
            (enumerate-interval 1 board-size)
          ))
          (queen-cols (- k 1))
        )  
      )
      )
    )
  (queen-cols board-size)
  )
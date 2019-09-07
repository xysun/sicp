; huffman encoding; variable length encoding; less bits for higher frequency letter
; representation: a binary tree, leaf holds symbol & weight (frequency), internal nodes contains all children and sum(weight)
; the sum(weight) in internal nodes are useful when generating the tree (because we need to select the pair of smallest weights)
; encode: from root, go left add 0, go right add 1
; generate: given a list of (symbol, frequency), find the pair of smallest weight, merge them as one node add back to the list, repeat

(define (make-leaf symbol weight)
  (list 'leaf symbol weight)
)

(define (leaf? object)
  (eq? (car object) 'leaf)
)

(define (symbol-leaf x)
  (cadr x)
)

(define (weight-leaf x)
  (caddr x)
)

; tree: left-branch, right-branch, all-symbols, all-weights
(define (make-code-tree left right)
  (list left
    right
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))
  )
)

(define (left-branch tree)
  (car tree)
)

(define (right-branch tree)
  (cadr tree)
)

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree))
)

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree))
)

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
            (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree)) ; start from root again
            (decode-1 (cdr bits) next-branch))
        )  
    )
  )

  (decode-1 bits tree)

)

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
    ((= bit 1) (right-branch branch))
    (else (error "bad bit" bit))
  )
)

; we maintain an ordered list of leaves
(define (adjoin-set x set)
  (cond ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))
  )
)

; given a list of (symbol, weight) pairs, construct an ordered list of leaves
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((first-pair (car pairs)))
      (adjoin-set (make-leaf (car first-pair) (cadr first-pair)) (make-leaf-set (cdr pairs))))
  )
)

; 2.67
(define sample-tree
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree 
      (make-leaf 'B 2)
      (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1))
    )
  )
)

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)) ; A D A B B C A

; 2.68
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree) (encode (cdr message) tree))
  )
)

(define (element-of-set? e s)
  (cond ((null? s) #f)
    ((equal? e (car s)) #t)
    (else (element-of-set? e (cdr s)))
  )
)

(define (encode-symbol symbol tree)
  (if (element-of-set? symbol (symbols tree))
    (if (leaf? tree)
      '()
      (if (element-of-set? symbol (symbols (left-branch tree)))
        (cons 0 (encode-symbol symbol (left-branch tree)))
        (cons 1 (encode-symbol symbol (right-branch tree)))))
    (error "symbol not in tree" symbol)
  )
) 

; 2.69
; pairs is a list of (symbol, weight) pairs
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
)

(define (successive-merge leaves)
  (define (aux  leaves)
    (cond ((null? leaves) '())
      ((null? (cdr leaves)) leaves)
      (else (let ((first (car leaves)) (second (cadr leaves)))
        (aux (adjoin-set (make-code-tree first second) (cddr leaves)))))
    )
  )

  (car (aux leaves)) ; extract the single root left
  
)

; 2.70
(define alphabet (list (list 'A 2) (list 'BOOM 1) (list 'GET 2) (list 'JOB 2) (list 'NA 16) (list 'SHA 3) (list 'YIP 9) (list 'WAH 1)))
(define rock-tree (generate-huffman-tree alphabet))

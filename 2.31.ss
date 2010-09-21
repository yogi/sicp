(define (square n)
  (* n n))

(define (tree-map fn tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (fn tree))
        (else (cons (tree-map fn (car tree))
                    (tree-map fn (cdr tree))))))

(define (square-tree tree) 
  (tree-map square tree))

(square-tree '(1 2 3 (4 5 (6) 7) 8)) 

; subsets

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))

(subsets '(1 2 3))
(subsets '(1 2 3 4))
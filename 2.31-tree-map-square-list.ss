(load "common.ss")

(define (square n)
  (* n n))

(define (tree-map fn tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (fn tree))
        (else (cons (tree-map fn (car tree))
                    (tree-map fn (cdr tree))))))

(define (square-tree tree) 
  (tree-map square tree))

(assert-equals '(1 4 9 (16 25 (36) 49) 64)
               (square-tree '(1 2 3 (4 5 (6) 7) 8))) 

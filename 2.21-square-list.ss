(load "common.ss")

(define (square-list items)
  (define (square x)
    (* x x ))
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(assert-equals '(1 4 9 16 25) 
               (square-list (list 1 2 3 4 5)))

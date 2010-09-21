(load "common.ss")

(define (square x) (* x x))

; 1
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(assert-equals '(1 4 9) (square-list '(1 2 3)))

; 2
(define (square-list items)
  (map (lambda (x) (square x)) 
       items))

(assert-equals '(1 4 9) (square-list '(1 2 3)))


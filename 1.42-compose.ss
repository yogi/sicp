(load "common.ss")

(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) 
  (+ x 1))

(define (square x)
  (* x x))

(assert-equals 49 ((compose square inc) 6))

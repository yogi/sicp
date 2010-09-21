(load "common.ss")

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) 
  (+ x 1))

(assert-equals 21 (((double (double double)) inc) 5))
(assert-equals 261 (((double (double (double double))) inc) 5))

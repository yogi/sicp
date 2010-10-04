(load "common.ss")

(define (accumulator initial)
  (lambda (x)
    (begin (set! initial (+ initial x))
           initial)))

(define a (accumulator 5))
(assert-equals 15 (a 10))
(assert-equals 25 (a 10))
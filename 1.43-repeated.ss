(load "common.ss")

(define (square x)
  (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated g n)
  (define (compose-iter fn counter)
    (if (>= counter n)
        fn
        (compose-iter (compose g fn) (+ counter 1))))
  (compose-iter g 1))

(assert-equals 625 ((repeated square 2) 5))

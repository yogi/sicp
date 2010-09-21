
(define (smooth fn)
  (define dx 0.00001)
  (define (average x y z)
    (/ (+ x y z) 3))
  (lambda (x) (average (fn (- x dx)) (fn x) (fn (+ dx x)))))

(define (repeated g n)
  (define (compose f g)
    (lambda (x) (f (g x))))  
  (define (compose-iter fn counter)
    (if (>= counter n)
        fn
        (compose-iter (compose g fn) (+ counter 1))))
  (compose-iter g 1))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))


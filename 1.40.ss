(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) 
  (+ x 1))

;((double inc) 7)

;(((double (double (double double))) inc) 5)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x)
  (* x x))

;((compose square inc) 6)

(define (repeated g n)
  (define (compose-iter fn counter)
    (if (>= counter n)
        fn
        (compose-iter (compose g fn) (+ counter 1))))
  (compose-iter g 1))

(define (iter fn from to)
  (if (< from to)
      (begin 
        (display (fn from))
        (iter fn (+ from 1) to))))
   
;((repeated inc 1) 5)
;((repeated inc 2) 5)
;((repeated inc 3) 5)
;((repeated inc 4) 5)

(define dx 0.001)

(define (smooth fn)
  (lambda (x) (average (fn (- x dx)) (fn x) (fn (+ dx x)))))

(define (average x y z)
  (/ (+ x y z) 3))

(define smooth-square (smooth square))

(define 2-smooth (repeated smooth 2))

((2-smooth square) 2)


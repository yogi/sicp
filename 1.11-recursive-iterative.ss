(load "common.ss")

(define (f-recursive n) 
  (if (< n 3) 
      n
      (+ (* 1 (f-recursive (- n 1))) 
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(assert-equals 1 (f-recursive 0))
(assert-equals 59 (f-recursive 6))

(define (f-iteratiive n)
  (define (iter fn0 fn1 fn2 fn3 n)
    (if (= n 0)
        fn3
        (iter fn1 
                fn2 
                fn3 
                (+ (* 1 fn3)
                   (* 2 fn2)
                   (* 3 fn1))
                (- n 1))))
  (if (< n 3) 
      n
      (iter 0 1 2 4 (- n 3))))

(assert-equals 0 (f-iteratiive 0))
(assert-equals 25 (f-iteratiive 5))
(assert-equals 59 (f-iteratiive 6))
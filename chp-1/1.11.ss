; recursive definition
(define (g n) 
  (if (< n 3) 
      n
      (+ (* 1 (f (- n 1))) 
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))


(define (f n)
  (define (f-iter fn0 fn1 fn2 fn3 n)
    (if (= n 0)
        fn3
        (f-iter fn1 
                fn2 
                fn3 
                (+ (* 1 fn3)
                   (* 2 fn2)
                   (* 3 fn1))
                (- n 1))))
  (if (< n 3) 
      n
      (f-iter 0 1 2 4 (- n 3))))

(define (show from to fn)
  (if (> from to)
      #t
      (begin
        (display (fn from)) 
        (newline)
        (show (+ from 1) to fn))))

(show 1 10 g)
(newline)
(show 1 10 f)
; simple implementation without using map
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(accumulate + 0 '(1 2 3 4 5 6))

(define (count-leaves tree)
  (accumulate (lambda (x y)
                (if (pair? x)
                    (+ y (count-leaves x))
                    (+ y 1)))
              0
              tree))

(count-leaves '(1))
(count-leaves '(1 2))
(count-leaves '(1 (2 (3) (4 5) (6) (7 (8 (9 (0)))))))

; using map
(define (count-leaves tree)
  (accumulate (lambda (leaves total) (+ leaves total))
              0
              (map (lambda (x)
                     (if (pair? x) 
                         (count-leaves x)
                         1))                         
                   tree)))

(count-leaves '(1 (2 (3) (4 5) (6) (7 (8 (9 (0)))))))


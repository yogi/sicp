(load "common.ss")

; simple implementation without using map
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (count-leaves tree)
  (accumulate (lambda (x y)
                (if (pair? x)
                    (+ y (count-leaves x))
                    (+ y 1)))
              0
              tree))

(assert-equals 1 (count-leaves '(1)))
(assert-equals 2 (count-leaves '(1 2)))
(assert-equals 10 (count-leaves '(1 (2 (3) (4 5) (6) (7 (8 (9 (0))))))))

; using map
(define (count-leaves tree)
  (accumulate (lambda (leaves total) (+ leaves total))
              0
              (map (lambda (x)
                     (if (pair? x) 
                         (count-leaves x)
                         1))                         
                   tree)))

(assert-equals 10 (count-leaves '(1 (2 (3) (4 5) (6) (7 (8 (9 (0))))))))


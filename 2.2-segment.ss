(define (average x y)
  (/ 2 (+ x y )))

(define (make-segment from to)
  (cons from to))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment)) (x-point (end-segment segment)))
              (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

(define (print-point x)
  (newline)
  (display "(")
  (display (x-point x))
  (display ",")
  (display (y-point x))
  (display ")"))

(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2))))
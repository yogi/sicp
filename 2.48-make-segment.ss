(define (make-segment start to)
  (list start to))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))
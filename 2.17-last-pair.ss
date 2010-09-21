(load "common.ss")

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(assert-equals '(12) (last-pair '(1 2 12)))

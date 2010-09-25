(load "common.ss")

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define frame (make-frame 0 1 2))

(assert-equals 0 (origin-frame frame))
(assert-equals 1 (edge1-frame frame))
(assert-equals 2 (edge2-frame frame))



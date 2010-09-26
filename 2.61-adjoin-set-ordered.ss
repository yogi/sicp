(load "common.ss")

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((<= x (car set)) 
         (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(assert-equals '(1) (adjoin-set 1 '()))
(assert-equals '(1 2 3 4 5) (adjoin-set 3 '(1 2 4 5)))
(assert-equals '(1 2 3 4 5) (adjoin-set 1 '(2 3 4 5)))
(assert-equals '(1 2 3 4 5) (adjoin-set 5 '(1 2 3 4)))
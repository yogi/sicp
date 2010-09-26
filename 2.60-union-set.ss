(load "common.ss")

; dependencies

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; union-set
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(assert-equals '() (union-set '() '()))
(assert-equals '(1) (union-set '() '(1)))
(assert-equals '(1) (union-set '(1) '()))
(assert-equals '(1 2) (union-set '(1) '(2)))
(assert-equals '(1 2) (union-set '(2 1) '(1 2)))

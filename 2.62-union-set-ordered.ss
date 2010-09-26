(load "common.ss")

; dependencies
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

; union-set
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else 
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2) 
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((> x1 x2)
                  (cons x2 (union-set set1 (cdr set2))))
                 (else 
                  (cons x1 (union-set set2 (cdr set1)))))))))

; tests
(assert-equals '() (union-set '() '()))
(assert-equals '(1) (union-set '() '(1)))
(assert-equals '(1) (union-set '(1) '()))
(assert-equals '(1 2) (union-set '(1) '(2)))
(assert-equals '(1 2) (union-set '(2) '(1)))
(assert-equals '(1 2) (union-set '(1 2) '(1 2)))
(assert-equals '(1 2 3) (union-set '(2) '(1 3)))

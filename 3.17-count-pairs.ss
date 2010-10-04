(load "common.ss")

(define (count-pairs seq)
  (define counted '())
  (define (iter x)
    (if (and (pair? x) (not (memq x counted)))
        (begin 
          (set! counted (cons x counted))
          (iter (car x))
          (iter (cdr x)))))
  (begin 
    (iter seq)
    (length counted)))

(define z '(a b c))
(assert-equals 3 (count-pairs z))

(set-car! (cdr z) (cddr z))
(assert-equals 3 (count-pairs z))

(set-car! z (cdr z))
(assert-equals 3 (count-pairs z))

(set-car! z z)
(assert-equals 3 (count-pairs z))


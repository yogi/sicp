(load "common.ss")

(define (same-parity . lst)
  (define (iter pred lst filtered)
    (cond ((null? lst) 
           filtered)
          ((pred (car lst))  
           (iter pred (cdr lst) (append filtered (list (car lst)))))
          (else 
           (iter pred (cdr lst) filtered))))
  (if (odd? (car lst))
      (iter odd? (cdr lst) (list (car lst)))
      (iter even? (cdr lst) (list (car lst)))))

(assert-equals '(1 3 5) (same-parity 1 2 3 4 5 6))
(assert-equals '(2 4 6) (same-parity 2 3 4 5 6 7))
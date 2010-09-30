
(define (equ? x y)
  (apply-generic 'equ? x y))

(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))

(put 'equ? '(rational rational)
     (lambda (x y) (and (= (numer x) (numer y))
                        (= (denom x) (denom y)))))
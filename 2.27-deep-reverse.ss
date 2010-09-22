(load "common.ss")

(define (deep-reverse x)
  (cond ((null? x) nil)
        ((pair? x)
         (append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))
        (else x)))

(assert-equals nil 
               (deep-reverse nil))

(assert-equals 1 
               (deep-reverse 1))

(assert-equals '(1) 
               (deep-reverse '(1)))

(assert-equals '((1)) 
               (deep-reverse '((1))))

(assert-equals '((1) 2) 
               (deep-reverse '(2 (1))))

(assert-equals '((1 2)) 
               (deep-reverse '((2 1))))

(assert-equals '(((4) ((3))) (2 1)) 
               (deep-reverse '((1 2) (((3)) (4)))))

(assert-equals '((4 3) (2 1))
               (deep-reverse '((1 2) (3 4))))

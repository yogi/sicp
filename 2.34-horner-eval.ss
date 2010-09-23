(load "common.ss")

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (horner-eval x coefficients)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms)
                   this-coeff))
              0
              coefficients))

(assert-equals 1 (horner-eval 2 '(1)))
(assert-equals 7 (horner-eval 2 '(1 3)))

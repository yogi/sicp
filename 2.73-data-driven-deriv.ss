(load "common.ss")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))(define (sum? x)
                                   (and (pair? x) (eq? (car x) '+)))

(define (sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(put 'deriv '+ sum)

(define (product exp var)
  (make-sum
   (make-product (multiplier exp)
                 (deriv (multiplicand exp) var))
   (make-product (deriv (multiplier exp) var)
                 (multiplicand exp))))

(put 'deriv '* product)

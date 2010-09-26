(load "common.ss")

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (exponentiation? exp)
  (and (pair? exp) (eq? '** (car exp))))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp) 
                       (make-exponentiation (base exp) 
                                            (- (exponent exp) 1)))) 
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (make-terms-sum terms)
  (split-terms make-sum terms))

(define (make-terms-product terms)
  (split-terms make-product terms)) 

(define (split-terms op terms)
  (if (null? (cddr terms))
      (op (car terms) (cadr terms))
      (op (car terms) (split-terms op (cdr terms)))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (augend s) 
  (if (null? (cdddr s))
      (caddr s)
      (make-terms-sum (cons (car s) (cddr s)))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (make-terms-product (cons (car p) (cddr p)))))

; instead of changing the selectors, I've written a procedure that correctly parenthesizes the exp, before it's passed to deriv
(define (parenthesize exp)
  (cond ((null? exp) null)
        ((symbol? exp) exp)
        ((number? exp) exp)
        ((null? (cdr exp)) exp)
        ((product? exp) (cons (list (car exp) '* (caddr exp))
                              (parenthesize (cdddr exp))))
        ((sum? exp) (append (list (car exp) '+) (parenthesize (cddr exp))))
        (else exp)))


(parenthesize '(x * 3 + (x + 2)))
(parenthesize '((x * 3) + (x + 2)))

(assert-equals '((x * 3) + (x + 2)) (parenthesize '(x * 3 + (x + 2))))
(assert-equals 4 (deriv '((x * 3) + (x + 2)) 'x))
(assert-equals 4 (deriv '((x + 3) * (x + 2)) 'x))
(assert-equals 4 (deriv '(x * 3 + (x + 2)) 'x))


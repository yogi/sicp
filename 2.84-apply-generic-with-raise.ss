
(define hierarchy '(integer rational real complex))                  

(define (apply-generic op . args)
  (define (index x seq)
    (cond ((null? seq)
           (error "not found" x))
          ((eq? x (car seq))
           0)
          (else (+ 1 (index x (cdr seq))))))
  
  (define (gt? type1 type2)
    (> (index type1 hierarchy)
       (index type2 hierarchy)))
  
  (define (coerce-args args)
    (let ((arg1 (car args))
          (arg2 (cadr args)))
    (if (eq? (type-tag arg1)
             (type-tag arg2))
        args
        (coerce-args (list (raise (car args)) (cadr args))))))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((coerced-args (if (gt? (car type-tags) (cadr type-tags))
                                      (coerce (list (cadr args) (car args)))
                                      (coerce (list (car args) (cadr args))))))
                (apply-generic op (car coerced-args) (cadr coerced-args)))
              (error "No method for these types" (list op type-tags)))))))

(define (raise x)
  (apply-generic 'raise x))

(puts 'raise 'integer
      (lambda (x) (make-rat x 1)))



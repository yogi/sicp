
(define (fold-right op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (fold-right op initial (cdr seq)))))

(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial seq))

(define (reverse-fold-right seq)
  (fold-right (lambda (cur rest) 
                (if (null? rest)
                    (list cur)                    
                    (append rest (list cur))))
              '()
              seq))

(reverse-fold-right '(1 2 3 4 5 7))

(define (reverse-fold-left seq)
  (fold-left (lambda (result cur) 
                (cons cur result))
              '()
              seq))

(reverse-fold-left '(1 2 3 4 5 7))
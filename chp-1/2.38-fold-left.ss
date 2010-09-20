
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

(fold-right / 1 '(1 2 3))
(fold-left / 1 '(1 2 3))

(fold-right list '() '(1 2 3))
(fold-left list '() '(1 2 3))

; operations have to be commutative, i.e. reordering the operands does not change the result, for the result of fold-left and fold-right to be the same for any sequence.

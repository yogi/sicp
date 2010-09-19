(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(map - '(1 2))
(map cdr '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(map cdr '((3) (6) (9) (12)))
(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

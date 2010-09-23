(load "common.ss")

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

(assert-equals '(-1 -2) (map - '(1 2)))
(assert-equals '((2 3)(5 6)(8 9)(11 12)) (map cdr '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
(assert-equals '(()()()()) (map cdr '((3) (6) (9) (12))))
(assert-equals '(22 26 30) (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))

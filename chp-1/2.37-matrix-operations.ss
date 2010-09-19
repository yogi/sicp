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

(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define v '(1 2 3 4))
(define w '(4 5 6 7))

(dot-product v w)

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) 
       m))

(matrix-*-vector m v)

(define (transpose m)
  (accumulate-n (lambda (x y)
                  (cons x y))
                '()
                m))
(transpose m)
  
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (matrix-*-vector n v)) 
         m)))

(define n '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(matrix-*-matrix m n)

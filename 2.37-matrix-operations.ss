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

(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))

; dot-product
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define v '(1 2 3 4))
(define w '(4 5 6 7))

(assert-equals 60 (dot-product v w))

; matrix-*-vector
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) 
       m))

(assert-equals '(30 56 80) (matrix-*-vector m v))

; transpose
(define (transpose m)
  (accumulate-n (lambda (x y)
                  (cons x y))
                '()
                m))
(assert-equals '((1 4 6) (2 5 7) (3 6 8) (4 6 9)) (transpose m))
  
; matrix-*-matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (matrix-*-vector n v)) 
         m)))

(define n '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(assert-equals '((14 32 50 68) (32 77 122 167) (44 107 170 233)) 
               (matrix-*-matrix m n))

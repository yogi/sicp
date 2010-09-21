(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(accumulate + 0 '(1 2 3 4 5))

(define (_map op seq)
  (accumulate (lambda (element rest)
                (cons (op element) rest))
              '()
              seq))

(define (square n)
  (* n n))

(_map square '(1 2 3 4 5 6))
                
(define (_append one two)
  (accumulate cons two one))

(_append '(1 2) '(3 4))

(define (_length seq)
  (accumulate (lambda (element count-so-far)
                (+ 1 count-so-far))
              0
              seq))

(_length '(1 2 3 4 5 89))
                
(define (_filter p seq)
  (accumulate (lambda (x y)
                (if (p x)
                    (cons x y)
                    y))
              '()
              seq))

(_filter odd? '(1 2 3 4 7 19 20 23))
(_filter even? '(1 2 3 4 7 19 20 23))

(define (_find pred seq)
  (accumulate (lambda (x y)
                (if (pred x)
                    x
                    y))
              '()
              seq))

(_find odd? '(2 4 6 7 8))
                    
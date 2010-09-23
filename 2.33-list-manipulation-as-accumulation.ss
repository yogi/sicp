(load "common.ss")

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(assert-equals 15 (accumulate + 0 '(1 2 3 4 5)))

; map
(define (_map op seq)
  (accumulate (lambda (element rest)
                (cons (op element) rest))
              '()
              seq))

(define (square n)
  (* n n))

(assert-equals '(1 4 9 16 25 36) (_map square '(1 2 3 4 5 6)))

; append
(define (_append one two)
  (accumulate cons two one))

(assert-equals '(1 2 3 4) (_append '(1 2) '(3 4)))

; length
(define (_length seq)
  (accumulate (lambda (element count-so-far)
                (+ 1 count-so-far))
              0
              seq))

(assert-equals 6 (_length '(1 2 3 4 5 89)))

; filter
(define (_filter p seq)
  (accumulate (lambda (x y)
                (if (p x)
                    (cons x y)
                    y))
              '()
              seq))

(assert-equals '(1 3 7 19 23) (_filter odd? '(1 2 3 4 7 19 20 23)))
(assert-equals '(2 4 20) (_filter even? '(1 2 3 4 7 19 20 23)))

; find
(define (_find pred seq)
  (accumulate (lambda (x y)
                (if (pred x)
                    x
                    y))
              '()
              seq))

(assert-equals 7 (_find odd? '(2 4 6 7 8)))
(assert-equals 2 (_find even? '(2 4 6 7 8)))

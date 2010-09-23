(load "common.ss")

(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start (enumerate-interval (+ 1 start) end))))

(enumerate-interval 1 5)

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))
                 
(define (flatmap op seq)
  (accumulate append
              '()
              (map op seq)))

(define (unique-pairs n)
  (flatmap (lambda(i)
             (map (lambda(j) (list j i))
                  (enumerate-interval 1 i)))
           (enumerate-interval 1 n)))

(unique-pairs 5)

(define (member? n seq)
  (cond ((null? seq) #f)
        ((= (car seq) n) #t)
        (else (member? n (cdr seq)))))

; :-P
(define (prime? n)
  (member? n '(2 3 5 7 11 13 17 19 23 29 31)))

(prime? 7)

(define (filter op seq)
  (accumulate (lambda (cur rest)
                (if (op cur)
                    (cons cur rest)
                    rest))
              '()
              seq))

(filter odd? '(1 2 3 4 5))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(prime-sum? '(1 1))
(prime-sum? '(1 2))
(prime-sum? '(1 3))

(define (prime-sum-pairs n)
  (map (lambda (pair) (let ((first (car pair))
                            (second (cadr pair)))
                        (list first second (+  first second))))
   (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 18)
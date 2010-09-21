(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start (enumerate-interval (+ 1 start) end))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))
                 
(define (flatmap op seq)
  (accumulate append
              '()
              (map op seq)))

(define (member? n seq)
  (cond ((null? seq) #f)
        ((= (car seq) n) #t)
        (else (member? n (cdr seq)))))

(define (filter op seq)
  (accumulate (lambda (cur rest)
                (if (op cur)
                    (cons cur rest)
                    rest))
              '()
              seq))

(define (ordered-triples n)
  (flatmap (lambda(i)
             (flatmap (lambda(j) 
                    (map (lambda (k) (list k j i))
                         (enumerate-interval 1 n)))
                  (enumerate-interval 1 n)))
           (enumerate-interval 1 n)))

;(unique-triples 5)

(define (sum-equal n)
  (lambda (triple)
    (= n (sum triple))))

(define (sum seq)
  (accumulate + 0 seq))

(define (triples-sum n sum)
   (filter (sum-equal sum) (ordered-triples n)))

(triples-sum 1 5)
(triples-sum 5 7)
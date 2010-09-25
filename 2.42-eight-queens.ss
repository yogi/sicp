(load "common.ss")

(define (flatmap op seq)
  (accumulate append
              '()
              (map op seq)))

(define (filter pred seq)
  (accumulate (lambda (x rest)
                (if (pred x)
                    (cons x rest)
                    rest))
              '()
              seq))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (enumerate-interval from to)
  (if (> from to)
      nil
      (cons from (enumerate-interval (+ 1 from) to))))

(define (member? elem seq)
  (accumulate (lambda (x found)
                (if (or found (equal? elem x))
                    #t
                    found))
              #f
              seq))

(define empty-board '())

(define (queens board-size) 
  (define (queen-cols k)  
    (if (= k 0) 
        (list empty-board)
        (filter 
         (lambda (positions) (safe? k positions)) 
         (flatmap 
          (lambda (rest-of-queens) 
            (map (lambda (new-row) 
                   (adjoin-position new-row k rest-of-queens)) 
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1)))))) 
  (queen-cols board-size))

(define (safe? col board)
  (define (check? horiz upper-diag lower-diag board)
    (if (null? board)
        #f
        (let ((cell (car board)))
          (or (member? cell (list horiz upper-diag lower-diag))
              (check? horiz (- upper-diag 1) (+ lower-diag 1) (cdr board))))))
  (not (check? (car board) (- (car board) 1) (+ (car board) 1) (cdr board))))

(define (adjoin-position new-row cur-col rest-of-queens)
  (cons new-row rest-of-queens))

;(adjoin-position (initial-guess 4) 4)

(assert-equals '((3 1 4 2) (2 4 1 3)) (queens 4))

(assert-equals '((4 2 5 3 1) (3 5 2 4 1) (5 3 1 4 2) (4 1 3 5 2) (5 2 4 1 3) (1 4 2 5 3) (2 5 3 1 4) (1 3 5 2 4) (3 1 4 2 5) (2 4 1 3 5)) 
               (queens 5))

(assert-equals '((5 3 1 6 4 2) (4 1 5 2 6 3) (3 6 2 5 1 4) (2 4 6 1 3 5))
               (queens 6))

(assert-equals 40 (length (queens 7)))

(assert-equals 92 (length (queens 8)))
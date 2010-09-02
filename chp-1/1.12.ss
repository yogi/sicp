
(define (cell col row)
  (if (= row 1) 
      (if (= col 1) 1 0)
      (+ (cell (- col 1) (- row 1)) (cell col (- row 1)))))

(define (row n)
  (cells 1 n))

(define (cells counter n)
  (if (<= counter n)
      (begin
        (display (cell counter n))
        (display " ")
        (cells (+ counter 1) n))))

(define (rows counter n)
  (if (<= counter n)
      (begin
        (row counter)
        (newline)
        (rows (+ counter 1) n))))

(define (pascal n)
  (rows 1 n))

(pascal 20)
(newline)
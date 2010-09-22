(load "common.ss")

(define (fringe lst)
  (if (null? (cdr lst))
      (if (pair? (car lst))
          (fringe (car lst))
          lst)
      (my-append (fringe (car lst)) (fringe (cdr lst)))))

(define (atom? a)
  (not (pair? a)))

(define (fringe lst)
  (if (atom? lst)
      (list lst)
      (if (null? (cdr lst))
          (fringe (car lst))
          (append (fringe (car lst)) (fringe (cdr lst))))))

(assert-equals '(1)
               (fringe '(1)))

(assert-equals '(1 2)
               (fringe '((1 2))))

(assert-equals '(1 2 3 4 5 6)
               (fringe '((1 2) (3 (4 5) 6))))

(define x '((1 2)(3 4)))

(assert-equals '(1 2 3 4 1 2 3 4) 
               (fringe (list x x)))
; 2.27 deep-reverse
(define (deep-reverse lst)
  (if (null? (cdr lst))
      (if (pair? (car lst))
          (list (deep-reverse (car lst)))
          lst)
      (my-append (deep-reverse (cdr lst)) (list (car lst)))))

(define (my-append first second)
  (if (null? first)
      second
      (cons (car first) (my-append (cdr first) second))))

(deep-reverse '((1 2) (3 4)))
(deep-reverse '(1 2))

; 2.28 fringe

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
          (my-append (fringe (car lst)) (fringe (cdr lst))))))

(fringe '(1))
(fringe '((1 2)))
(fringe '((1 2) (3 (4 5) 6)))
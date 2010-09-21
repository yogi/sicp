(define (square x)
  (* x x ))

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (my-append first second)
  (if (null? first)
      second
      (cons (car first) (append (cdr first) second))))

(define (my-reverse lst)
  (if (null? (cdr lst))
      lst
      (my-append (my-reverse (cdr lst)) (list (car lst)))))

(my-reverse (list 1 2))
(my-append (list 1 2) (list 3 4))
(last-pair (list 1 2))

(define (same-parity . lst)
  (define (iter pred lst filtered)
    (cond ((null? lst) 
           filtered)
          ((pred (car lst))  
           (iter pred (cdr lst) (append filtered (list (car lst)))))
          (else 
           (iter pred (cdr lst) filtered))))
  (if (odd? (car lst))
      (iter odd? (cdr lst) (list (car lst)))
      (iter even? (cdr lst) (list (car lst)))))

(same-parity 1 2 3 4 5 6)
(same-parity 2 3 4 5 6 7)

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4 5))

(define (sq-list items)
  (map (lambda (x) (* x x))
       items))
(sq-list (list 1 2 3 4 5))

(define (sq-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items '()))

(sq-list (list 1 2 3 4 5))

(define (sq-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer (list (square (car things)))))))
  (iter items '()))

(sq-list (list 1 2 3 4 5))

(define (my-for-each proc lst)
  (if (null? lst)
      (newline)
      (let ()
        (proc (car lst))
        (my-for-each proc (cdr lst)))))

(my-for-each (lambda (x) (newline) (display x)) (list 1 2 3))

(car (cdr (caddr '(1 3 (5 7) 9))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
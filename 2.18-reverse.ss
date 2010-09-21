(load "common.ss")

; can't redfine append, so using "_" suffix in such cases 
(define (append_ first second)
  (if (null? first)
      second
      (cons (car first) (append_ (cdr first) second))))

(define (reverse_ lst)
  (if (null? (cdr lst))
      lst
      (append_ (reverse_ (cdr lst)) (list (car lst)))))

(assert-equals '(2 1) 
               (reverse_ '(1 2)))

(assert-equals '(1 2 3 4) 
               (append_ '(1 2) '(3 4)))


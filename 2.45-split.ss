(require (planet soegaard/sicp:2:1/sicp))

(define (split op-a op-b)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (op-a painter (op-b smaller smaller)))))
  splitter)

(define right-split (split below beside))
(define up-split (split beside below))

(paint (up-split einstein 4))
(paint (right-split einstein 4))


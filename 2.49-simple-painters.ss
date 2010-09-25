(require (planet soegaard/sicp:2:1/sicp))
(load "2.46-vector-ops.ss")
(load "2.47-frame-selectors.ss")



(define one 0.99)

(define unit-square (make-frame (make-vect 0 0)
                                (make-vect one 0)
                                (make-vect 0 one)))

; outline
(define (outline-painter frame)
  (segments->painter 
   (let ((origin (origin-frame frame)) 
         (bottom-right (edge1-frame frame))
         (top-left (edge2-frame frame))
         (top-right (add-vect (edge1-frame frame) (edge2-frame frame))))                    
     (list (make-segment origin bottom-right)
           (make-segment bottom-right top-right)
           (make-segment top-right top-left)
           (make-segment top-left origin)))))

(paint (outline-painter unit-square))

; x
(define (x-painter frame)
  (segments->painter 
   (let ((origin (origin-frame frame)) 
         (bottom-right (edge1-frame frame))
         (top-left (edge2-frame frame))
         (top-right (add-vect (edge1-frame frame) (edge2-frame frame))))                    
     (list (make-segment origin top-right)
           (make-segment top-left bottom-right)))))

(paint (x-painter unit-square))

; diamond

(define (diamond-painter frame)
  (segments->painter 
   (let ((origin (origin-frame frame)) 
         (bottom-right (edge1-frame frame))
         (top-left (edge2-frame frame))
         (top-right (add-vect (edge1-frame frame) (edge2-frame frame))))
     (let ((left-mid (midpoint-segment origin top-left))
           (top-mid (midpoint-segment top-left top-right))
           (right-mid (midpoint-segment top-right bottom-right))
           (bottom-mid (midpoint-segment bottom-right origin)))
       (list (make-segment bottom-mid left-mid)
             (make-segment left-mid top-mid)
             (make-segment top-mid right-mid)
             (make-segment right-mid bottom-mid))))))

(paint (diamond-painter unit-square))
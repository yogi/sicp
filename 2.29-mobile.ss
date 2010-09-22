

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define left (make-branch 2 10))
(define right (make-branch 3 4))

(left-branch left)

(define (branch-structure branch)
  (cdr branch))

(define (branch-length branch)
  (car branch))

(define (atom? a)
  (not (pair? a)))

(define (weight branch)
  (if (atom? (branch-structure branch))
      (* (branch-length branch)
         (branch-structure branch))
      (* (branch-length branch)
         (total-weight (branch-structure branch)))))

(define (total-weight mobile)
  (+ (weight (left-branch mobile))
     (weight (right-branch mobile))))

(define (balanced? mobile)
  (= (weight (left-branch mobile))
     (weight (right-branch mobile))))

(define l (make-branch 2 10))
(define r (make-branch 3 4))
(define x (make-branch 4 5))

(define m (make-mobile l r))
(define n (make-mobile l x))

(total-weight m)
(balanced? m)

(total-weight n)
(balanced? n)
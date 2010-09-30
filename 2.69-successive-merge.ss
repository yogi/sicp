(load "common.ss")

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree) 
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (member? x seq)
  (cond ((null? seq) false)
        ((eq? (car seq) x) true)
        (else (member? x (cdr seq)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

   
(define (encode-symbol symbol tree)
  (cond ((and (leaf? tree) 
              (eq? symbol (symbol-leaf tree)))
         '())
        ((member? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((member? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "should not get here" symbol))))
                  
; implementation
(define (generate-huffman-tree pairs)
  (define (merge leafs)
    (let ((merged (make-code-tree (car leafs) (cadr leafs))))
      (if (null? (cddr leafs))
          (list merged)
          (adjoin-set merged (cddr leafs)))))
  (define (successive-merge leafs)
    (if (null? (cdr leafs))
        leafs
        (successive-merge (merge leafs))))
  (successive-merge (make-leaf-set pairs)))

; tests
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'C 1)
                                   (make-leaf 'D 1)))))
(generate-huffman-tree '((B 1) (A 2)))
(generate-huffman-tree '((C 1) (B 2) (A 3)))
(generate-huffman-tree '((a 8) (b 3) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1)))

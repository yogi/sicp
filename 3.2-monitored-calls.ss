(define (make-monitored fn)
  (let ((count 0))
    (lambda (arg)
      (if (eq? arg 'how-many-calls?)
          count
          (begin
            (set! count (+ count 1))
            (fn arg))))))

(define s (make-monitored sqrt))

(s 100)
(s 100)

(s 'how-many-calls?)
(define nil '())

(define (assert-equals expected actual)
  (if (equal? expected actual)
      #t
      (let ()
        (display "expected: ")
        (display expected)
        (newline)
        (display "     got: " )
        (display actual)
        (newline))))
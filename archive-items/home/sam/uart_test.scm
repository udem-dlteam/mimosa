;; The mimosa project

(define msg "This is a long message to send")


(define (do-test)
  (map (lambda (c)
         (if (char? c)
             (uart-write 1 c)
             0))
       (list msg)))

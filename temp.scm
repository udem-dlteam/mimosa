(define-macro (write-uint vec offset val w)
                    `(begin
                       ,@(map
                           (lambda (i)
                             `(vector-set! ,vec (+ offset ,i) (fxand #xFF (arithmetic-shift val ,(- 0 (* i 8))))))
                           (iota w))))


(define (write-uint32 vec offset val)
 (write-uint vec offset val 4)
 vec)
(define v (vector 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

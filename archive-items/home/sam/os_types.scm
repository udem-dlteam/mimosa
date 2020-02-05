(define (bcd->binary byte)
 (+ (* 10 (fxarithmetic-shift-right byte 4)) (fxand byte #x0F)))


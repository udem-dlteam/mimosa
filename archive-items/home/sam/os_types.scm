;; The mimosa project

(define DEBUG-WRITE-PORT #xE9)

(define (bcd->binary byte)
 (+ (* 10 (fxarithmetic-shift-right byte 4)) (fxand byte #x0F)))

(define (char-debug-write char)
 (outb char DEBUG-WRITE-PORT))

(define (debug-write str)
  (let ((str (if (not (string? str))
                 (number->string str)
                 str)))
    (let ((str (string-append "[SCM] " str "\n")))
      (begin
        (map (lambda (c)
               (if (char? c)
                   (char-debug-write c)))
             (string->list str))
        str))))

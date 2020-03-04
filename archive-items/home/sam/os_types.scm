;; The mimosa project

(define DEBUG-WRITE-PORT #xE9)

(define (bcd->binary byte)
 (+ (* 10 (fxarithmetic-shift-right byte 4)) (fxand byte #x0F)))

(define (debug-write obj)
 (cond ((string? obj)
        (str-debug-write obj))
       ((vector? obj)
        (vector-debug-write obj))
       ((number? obj)
        (number-debug-write obj))
       (else
        (str-debug-write (string obj)))))

(define (str-debug-write str)
  (let ((str (string-append "[SCM] " str "\n")))
    (begin
      (map (lambda (c)
             (if (char? c)
                 (char-debug-write c)))
           (string->list str))
      str)))

(define (number-debug-write nbr)
    (str-debug-write (number->string nbr)))

(define (vector-debug-write vect)
  (str-debug-write (fold-right (lambda (cur rest)
                (string-append (number->string cur) " " rest))
              ""
              (vector->list vect))))

(define (char-debug-write char)
 (outb char DEBUG-WRITE-PORT))

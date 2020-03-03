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
 (let ((l (vector-length vect))
       (lst-rpr (vector->list vect)))
  (debug-write (vector-ref vect 0))
  (debug-write (vector-ref vect 1))
  (debug-write (vector-ref vect 2))))

(define (char-debug-write char)
 (outb char DEBUG-WRITE-PORT))

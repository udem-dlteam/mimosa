(define-library (debug)
 (import (gambit) (low-level))
 (export debug-write)
 (begin
   (define DEBUG-WRITE-PORT #xE9)

   (define (debug-write obj)
     (cond ((string? obj)
            (str-debug-write obj))
           ((vector? obj)
            (vector-debug-write obj))
           ((list? obj)
            (for-each debug-write obj))
           ((number? obj)
            (number-debug-write obj))
           ((symbol? obj)
            (debug-write (symbol->string obj)))
           ((boolean? obj)
            (bool-debug-write obj))
           (else
             (str-debug-write (string obj)))))

    (define (bool-debug-write obj)
     (debug-write (if obj
                      "true"
                      "false")))

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
     ; This is not great, as it only deals with vector of numbers
      (str-debug-write (fold-right (lambda (cur rest)
                    (string-append (number->string cur) " " rest))
                  ""
                  (vector->list vect))))

    (define (char-debug-write char)
     (outb char DEBUG-WRITE-PORT))
   ))

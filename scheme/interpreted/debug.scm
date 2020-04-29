; Mimosa
; Université de Montréal
; Marc Feeley, Samuel Yvon
(define-library
  (debug)
  (import (gambit) (low-level))
  (export debug-write)
  (begin
    (define ENABLE-DEBUG? #t)
    ; The debug port is the classic E9 fake device that outputs to the console
    ; that runs qemu / bochs
    ; see https://lists.gnu.org/archive/html/qemu-devel/2005-01/msg00169.html
    (define DEBUG-WRITE-PORT #xE9)

    ; Ouputs to the console that runs the emulator
    ; Formats the data according to the type
    ; Is there define-multi in gambit?
    (define (debug-write obj)
      (if ENABLE-DEBUG?
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
                ((pair? obj)
                 (pair-debug-write obj))
                ((procedure? obj)
                 (procedure-debug-write obj))
                (else
                  (str-debug-write (string obj))))))

    (define (pair-debug-write obj)
      (let ((a (car obj))
            (b (cdr obj)))
        (debug-write a)
        (debug-write b)))

    (define (procedure-debug-write obj)
      (debug-write "procedure"))

    (define (bool-debug-write obj)
      (debug-write (if obj
                       "true"
                       "false")))

    (define (str-debug-write str)
      (let ((str (string-append "[SCM] " str "\n")))
        (map (lambda (c)
               (if (char? c)
                   (char-debug-write c)))
             (string->list str))
        str))

    (define (number-debug-write nbr)
      (str-debug-write (number->string nbr)))

    (define (vector-debug-write vect)
      ; This is not great, as it only deals with vector of numbers
      (str-debug-write
        (fold-right (lambda (cur rest) (string-append (number->string cur) " " rest))
                    ""
                    (vector->list vect))))

    (define (char-debug-write char)
      (outb char DEBUG-WRITE-PORT))

    ))

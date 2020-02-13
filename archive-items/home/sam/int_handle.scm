;; The mimosa project

(define KEYBOARD-INT #x1)
(define UART-INT #x2)

(define INT-WITH-ARG-TABLE
  (list (cons KEYBOARD-INT handle-kbd-int)
        (cons UART-INT handle-uart-int)))

(define (handle-int-without-arg int-no)
 (display "TODO"))

(define (handle-int-with-arg int-no int-val)
 (begin
  ((assocv int-no INT-WITH-ARG-TABLE) int-val)))

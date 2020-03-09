;; The mimosa project

(import (ide) (uart) (keyboard))

(define KEYBOARD-INT #x1)
(define UART-INT #x2)
(define IDE-INT #x3)

(define INT-WITH-ARG-TABLE
  (list (cons KEYBOARD-INT handle-kbd-int)
        (cons UART-INT handle-uart-int)
        (cons IDE-INT handle-ide-int)))

(define (handle-int int-no args)
    (let ((fn (assocv int-no INT-WITH-ARG-TABLE)))
     (apply fn args)))

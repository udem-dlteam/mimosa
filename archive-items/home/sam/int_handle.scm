;; The mimosa project

(define KEYBOARD-INT #x1)
(define UART-INT #x2)

(define INT-WITH-ARG-TABLE
  (list (cons KEYBOARD-INT handle-kbd-int)
        (cons UART-INT handle-uart-int)))

(define (handle-int int-no args)
    (let ((fn (assocv int-no INT-WITH-ARG-TABLE)))
     (apply fn args)))

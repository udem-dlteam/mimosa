;; The mimosa project

(define KEYBOARD-INT #x1)




(define INT-WITH-ARG-TABLE
 (list (cons KEYBOARD-INT handle-kbd-int)))


(define (handle-int-without-arg int-no)
 (display "TODO"))


(define (handle-int-with-arg int-no int-val)
 ((assocv int-no INT-WITH-ARG-TABLE) int-val))

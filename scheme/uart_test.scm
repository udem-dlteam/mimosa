; Mimosa
; Université de Montréal
; Marc Feeley, Samuel Yvon
(define msg "This is a long message to send")

(define (write-test)
 (begin
  (map (lambda (c)
         (if (char? c)
             (uart-write 1 c)
             0))
       (string->list msg)))
  "DONE")

; UART util to get all attached material
(define (get-material)
  (let ((idx (map ++ (iota 4))))
    (map (lambda (i)
           (identify-material i)) idx)))

(define (test-reader-thread com-port chr)
 (let* ((port-data (get-port-data com-port))
        (uart-endpoint (port-data-get-repl-endpoint port-data)))
  (write-char chr uart-endpoint)))

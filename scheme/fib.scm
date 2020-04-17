; Mimosa
; Université de Montréal
; Marc Feeley, Samuel Yvon
(declare
  (standard-bindings)
  (extended-bindings)
  (not safe)
  (block))

(define (fib n)
  (if (fx< n 2)
      n
      (fx+ (fib (fx- n 1))
           (fib (fx- n 2)))))

(pretty-print (time (fib 40)))

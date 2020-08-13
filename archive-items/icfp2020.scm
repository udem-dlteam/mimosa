;; Compute 1 + 2 + 3 ... + n
;; Call as (sum n 0)
(define (sum n s)
 (if (= n 0)
  s
  (sum (- n 1) (+ n s))))

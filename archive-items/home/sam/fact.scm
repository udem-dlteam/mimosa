(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(pretty-print (time (fact 1000)))

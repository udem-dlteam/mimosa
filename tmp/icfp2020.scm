;; Get the last element
;; out of a list
(define (last x)
  (if (pair? (cdr x))
      (last (cdr x))
      (car x)))

;; Test out with:
;; (last (iota 100000))

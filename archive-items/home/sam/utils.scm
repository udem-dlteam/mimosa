;; The mimosa project

(define (assock key tbl)
 (car (assoc key tbl)))

(define (assocv key tbl)
  (cdr (assoc key tbl)))

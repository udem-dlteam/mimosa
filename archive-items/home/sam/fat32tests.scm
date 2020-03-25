(define-library (fat32tests)
 (import (gambit) (debug) (fat32))
 (export
  (fat32-run-tests)
  )
 (begin
  (define test-list '())
  (define-macro (define-test declaration body)
                `(begin
                   (set! test-list (cons ,(car declaration)) test-list)
                   (define ,(car declaration) ,body)))

  (define-test (open-file)
   ((debug-write "OPEN FILE")))


    (define (fat32-run-tests)
     (debug-write (length test-list)))
  )
 )

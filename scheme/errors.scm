; Mimosa
; Université de Montréal
; Marc Feeley, Samuel Yvon
(define-library
  (errors)
  (import (gambit))
  (export
    ERR-ARGS
    ERR-EOF
    ERR-FNF
    ERR-HWD
    ERR-NO-MORE-ENTRIES
    ERR-PATH
    )
  (begin
    (define-macro
      (tags . codes)
      `(begin
         ,@(map
             (lambda (sym) `(define ,sym (quote ,sym)))
             codes)))
    (tags
      ERR-ARGS
      ERR-EOF
      ERR-FNF
      ERR-HWD
      ERR-NO-MORE-ENTRIES
      ERR-PATH
      )
   ))

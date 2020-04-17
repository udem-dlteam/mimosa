; The mimosa project
; TODO: I do not know if a table is sync safe
; Ill assume yes for now
(define-library (errors)
                (import (gambit))
                (export
                  ERR-EOF
                  ERR-FNF
                  ERR-PATH
                  ERR-NO-MORE-ENTRIES
                  ERR-ARGS
                  ERR-HWD
                  )
                (begin
                  (define-macro (tags . codes)
                                `(begin
                                   ,@(map
                                       (lambda (sym) `(define ,sym (quote ,sym)))
                                       codes)
                                   ))
                  (tags
                    ERR-EOF
                    ERR-FNF
                    ERR-PATH
                    ERR-NO-MORE-ENTRIES
                    ERR-ARGS
                    ERR-HWD)
                  )
                )

; | --- Does not seem to work
; v
;
; (define-macro (errors . codes)
;   `(define-library (errors)
;                    (import (gambit))
;                    (export
;                         ,@codes
;                    )
; 									 (begin
; 										 ,@(map (lambda (sym) `(define ,sym (gensym))) codes)
; 										 )))
; (errors 
; 	ERR-EOF 
; 	ERR-FNF 
; 	ERR-PATH 
; 	ERR-NO-MORE-ENTRIES 
; 	ERR-ARGS 
; 	ERR-HWD 
; 	)
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

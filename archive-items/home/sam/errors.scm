(define-macro (errors . codes)
  `(define-library (errors)
                   (import (gambit))
                   (export
                        ,@codes
                   )
									 (begin
										 (begin ,@(map (lambda (sym) `(define ,sym (quote ,sym))) codes))
										 )))

(errors 
	ERR-EOF 
	ERR-FNF 
	ERR-PATH 
	ERR-NO-MORE-ENTRIES 
	ERR-ARGS 
	ERR-HWD 
	)

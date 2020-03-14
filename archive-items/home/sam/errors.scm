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
   (define ERR-EOF 'ERR-EOF)
   (define ERR-FNF 'ERR-FNF)
   (define ERR-PATH 'ERR-PATH)
   (define ERR-NO-MORE-ENTRIES 'ERR-NO-MORE-ENTRIES)
   (define ERR-ARGS 'ERR-INVALID-ARGS)
   (define ERR-HWD 'ERROR-HARDWARE)
))

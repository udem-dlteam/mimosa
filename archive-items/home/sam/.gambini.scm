; The MIMOSA project
; Scheme executor bridge
;(include "~~lib/_asm#.scm")
;(include "~~lib/_x86#.scm")
;(include "~~lib/_codegen#.scm")
(define MEMORY_FILE_NAME "mem.txt")


;(define inb ;; parameter: port number
  ;(asm
   ;(lambda (cgc)
     ;(x86-mov   cgc (x86-edx) (x86-mem 4 (x86-esp)))
     ;(x86-sar   cgc (x86-edx) (x86-imm-int 2))
     ;(x86-mov   cgc (x86-eax) (x86-imm-int 0))
     ;(x86-in-dx cgc (x86-al))
     ;(x86-shl   cgc (x86-eax) (x86-imm-int 2))
     ;(x86-ret   cgc)
     ;)))

;(define outb ;; parameters: value and port number
  ;(asm
   ;(lambda (cgc)
     ;(x86-mov   cgc (x86-edx) (x86-mem 8 (x86-esp)))
     ;(x86-sar   cgc (x86-edx) (x86-imm-int 2))
     ;(x86-mov   cgc (x86-eax) (x86-mem 4 (x86-esp)))
     ;(x86-sar   cgc (x86-eax) (x86-imm-int 2))
     ;(x86-out-dx cgc (x86-al))
     ;(x86-shl   cgc (x86-eax) (x86-imm-int 2))
     ;(x86-ret   cgc)
     ;)))

(define RTC_PORT_ADDR #x70)
(define RTC_PORT_DATA #x71)

(define RTC_SEC  0)
(define RTC_MIN  2)
(define RTC_HOUR 4)

;(define (get-RTC_SEC)
  ;(outb RTC_SEC RTC_PORT_ADDR) ;; select seconds reg
  ;(inb RTC_PORT_DATA))         ;; read the register

(define (fact n) (if (= n 1)
                     1
                     (* n (fact (- n 1)))))

(define (mimosa_interrupt_handler)
  (let ((read_val (read-i8 #f #x300000)))
    (display (string-append "Reading " (number->string read_val) "\n"))
    (write-i8 #f (+ #x300000 512) 1)
    (write-i8 #f (+ #x300000 512 1) (fact read_val))))

(##interrupt-vector-set! 5 mimosa_interrupt_handler)

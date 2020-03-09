; The MIMOSA project
; Scheme executor bridge
(import (ide) (disk) (utils) (fat32) (low-level) (debug))

(define SHARED-MEMORY-AREA #x300000)

; (define RTC_PORT_ADDR #x70)
; (define RTC_PORT_DATA #x71)

; (define RTC_SEC  0)
; (define RTC_MIN  2)
; (define RTC_HOUR 4)

; (define (get-RTC_SEC)
;   (outb RTC_SEC RTC_PORT_ADDR) ;; select seconds reg
;   (inb RTC_PORT_DATA))         ;; read the register

;;;----------------------------------------------------
;;;                      IMPORTs 
;;;----------------------------------------------------

(load "mimosa_io.scm")
(load "intr.scm")
(load "int_handle.scm") ; must be loaded after all drivers

;;;----------------------------------------------------
;;;                    INIT SYS 
;;;----------------------------------------------------

(define int-mutex (make-mutex))
(define int-condvar (make-condition-variable))

(for-each (o uart-do-init ++) (iota 4))

(ide#setup)
(ide#switch-over-driver)

(init-disks)
(define main-disk (car disk-list))

;;;----------------------------------------------------
;;;                 INTERRUPT HANDLING 
;;;----------------------------------------------------

(define unhandled-interrupts (open-vector))

(define (mimosa-interrupt-handler)
  (let* ((int-no (read-iu8 #f SHARED-MEMORY-AREA))
         (arr-len (read-iu8 #f (+ SHARED-MEMORY-AREA 1)))
         (params (map (lambda (n)
                        (read-iu8 #f (+ SHARED-MEMORY-AREA 2 n)))
                      (iota arr-len))))
    (let ((packed (list int-no params)))
      (write packed unhandled-interrupts)
      (force-output unhandled-interrupts))))

;;;----------------------------------------------------
;;;                  INTERRUPT WIRING 
;;;----------------------------------------------------


(##interrupt-vector-set! 5 mimosa-interrupt-handler)


;;;----------------------------------------------------
;;;              INTERRUPT EXEC ROUTINE 
;;;----------------------------------------------------

(define (exec)
    ; sleep if nothing
    (let* ((packed (read unhandled-interrupts)))
      (handle-int (car packed) (cadr packed))
      (exec)))

(thread-start! (make-thread exec "int execution g-tread"))

(define (t)
 (fat32-tests main-disk))

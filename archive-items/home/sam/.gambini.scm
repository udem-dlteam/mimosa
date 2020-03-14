; The MIMOSA project
; Scheme executor bridge
(import (ide)
        (disk)
        (utils)
        (fat32)
        (low-level)
        (debug))

(define reader-offset 0)
(define SHARED-MEMORY-AREA #x300000)
(define SHARED-MEMORY-AREA-LEN 512)

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
; (load "edit.scm")
(load "int_handle.scm") ; must be loaded after all drivers

;;;----------------------------------------------------
;;;                    INIT SYS 
;;;----------------------------------------------------

(define int-mutex (make-mutex))

; (for-each (o uart-do-init ++) (iota 4))

(ide#setup)
(ide#switch-over-driver)

(init-disks)
(define main-disk (car disk-list))

(define (t) (f-tests main-disk))

;;;----------------------------------------------------
;;;                 INTERRUPT HANDLING 
;;;----------------------------------------------------

(define (read-at . offset)
 (+ SHARED-MEMORY-AREA (modulo (fold + 0 offset) SHARED-MEMORY-AREA-LEN)))

(define (erase-and-move! total-len)
  (for-each (lambda (i)
              (write-i8 #f (read-at i reader-offset) #x00))
            (iota total-len))
  (set! reader-offset (modulo (+ reader-offset total-len) SHARED-MEMORY-AREA-LEN)))

(define unhandled-interrupts (open-vector))

(define (mimosa-interrupt-handler)
  (let pmp ()
    (let ((int-no (read-iu8 #f (read-at reader-offset))))
      (if (fx= 0 int-no)
            #t; stop
          (let* ((arr-len (read-iu8 #f (read-at 1 reader-offset)))
                 (params (map (lambda (n) (read-iu8 #f (read-at 2 n reader-offset))) (iota arr-len)))
                 (total-len (+ 2 arr-len)))
            (write (list int-no params) unhandled-interrupts)
            (force-output unhandled-interrupts)
            (erase-and-move! total-len) ; allow execution asap
            (pmp))
          ))))

;;;----------------------------------------------------
;;;                  INTERRUPT WIRING 
;;;----------------------------------------------------


(##interrupt-vector-set! 5 (lambda () #t))


;;;----------------------------------------------------
;;;              INTERRUPT EXEC ROUTINE 
;;;----------------------------------------------------

(define (exec)
    ; sleep if nothing
    (let* ((packed (read unhandled-interrupts)))
      (handle-int (car packed) (cadr packed))
      (exec)))
    
(define (idle)
  (mimosa-interrupt-handler)
  (thread-yield!) 
  (idle))

(thread-start! (make-thread exec "int execution g-tread"))
(thread-start! (make-thread idle "Mimosa idle green thread"))

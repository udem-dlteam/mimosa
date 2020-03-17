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
(load "edit.scm")
(load "int_handle.scm") ; must be loaded after all drivers

;;;----------------------------------------------------
;;;                    INIT SYS 
;;;----------------------------------------------------

(define int-mutex (make-mutex))
(define int-condvar (make-condition-variable))

; (for-each (o uart-do-init ++) (iota 4))


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

(##interrupt-vector-set! 5 (lambda () 
                            ; Signal the int pump threadi
                            ; to pump the fifo into the scheme fifo
                            (mutex-lock! int-mutex)
                            (condition-variable-signal! int-condvar)
                            (mutex-unlock! int-mutex)))


;;;----------------------------------------------------
;;;              INTERRUPT EXEC ROUTINE 
;;;----------------------------------------------------

(define (exec)
    ; sleep if nothing
    (let* ((packed (read unhandled-interrupts)))
      (handle-int (car packed) (cadr packed))
      (exec)))
    
(define (int-clear)
  (mutex-unlock! int-mutex int-condvar 30) ; wait on condvar, timeout
  (mimosa-interrupt-handler)
  (int-clear))

(define (idle)
  (thread-yield!) 
  (idle))

(thread-start! (make-thread exec "int execution g-tread"))
(thread-start! (make-thread int-clear "Mimosa interrupt clearing thread"))
(thread-start! (make-thread idle "Mimosa idle green thread"))


;;;----------------------------------------------------
;;;                     INIT SYSTEM  
;;;----------------------------------------------------

(ide#setup)
(ide#switch-over-driver)

(init-disks)
(define main-disk (car disk-list))
(mount-partitions disk-list)
(define fs (car filesystem-list))

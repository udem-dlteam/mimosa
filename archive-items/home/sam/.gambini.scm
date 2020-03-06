; The MIMOSA project
; Scheme executor bridge
(include "queue.scm")
(import (utils) (low-level))

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

(load "utils.scm")
(load "mimosa_io.scm")
(load "intr.scm")
(load "os_types.scm")
(load "uart.scm")
(load "keyboard.scm")
(load "ide.scm")
(load "disk.scm")
(load "int_handle.scm") ; must be loaded after all drivers
(load "ide_tests.scm")

;;;----------------------------------------------------
;;;                    INIT SYS 
;;;----------------------------------------------------

(define int-mutex (make-mutex))
(define int-condvar (make-condition-variable))

(for-each (o uart-do-init ++) (iota 4))

(ide-setup)
(ide-switch-over-driver)
(init-disks)
(define main-disk (car disk-list))
(define main-dev (disk-ide-device main-disk))

;;;----------------------------------------------------
;;;                 INTERRUPT HANDLING 
;;;----------------------------------------------------

(define unhandled-interrupts (make 255))

(define (mimosa-interrupt-handler)
  (let* ((int-no (read-iu8 #f SHARED-MEMORY-AREA))
         (arr-len (read-iu8 #f (+ SHARED-MEMORY-AREA 1)))
         (params (map (lambda (n)
                        (read-iu8 #f (+ SHARED-MEMORY-AREA 2 n)))
                      (iota arr-len))))
    ; (mutex-lock! int-mutex)
    (let ((packed (list int-no params)))
      (push packed unhandled-interrupts))))
    ; (condition-variable-signal! int-condvar)
    ; (mutex-unlock! int-mutex)))

;;;----------------------------------------------------
;;;                  INTERRUPT WIRING 
;;;----------------------------------------------------


(##interrupt-vector-set! 5 mimosa-interrupt-handler)


;;;----------------------------------------------------
;;;              INTERRUPT EXEC ROUTINE 
;;;----------------------------------------------------

(define (exec)
  (begin
    (if (not (empty? unhandled-interrupts))
        (begin
          (let* ((packed (pop unhandled-interrupts)))
            (handle-int (car packed) (cadr packed))
            (exec)))
        ; Spin loop for now? Probably gonna be worth
        ; using thread mecanisms
        (begin
          ; (mutex-lock! int-mutex)
          ; (debug-write "Unlock on condvar")
          ; (mutex-unlock! int-mutex int-condvar) ;; This is the wait
          ; (debug-write "Condvar unlocked")
          (exec)))))

(thread-start! (make-thread exec "int execution g-tread"))

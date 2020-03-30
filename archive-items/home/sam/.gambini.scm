; The MIMOSA project
; Scheme executor bridge
(import (errors)
        (keyboard)
        (ide)
        (disk)
        (utils)
        (rtc)
        (fat32)
        (low-level)
        (uart)
        (debug))

(define (reboot)
  (let wait-loop ()
    (let ((temp (inb #x64)))
      (if (mask temp #x01)
          (inb #x60)
          (if (mask temp #x02)
              (wait-loop)))))
  (outb #xFE #x64))

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

(load "edit.scm")

;;;----------------------------------------------------
;;;                    INIT SYS
;;;----------------------------------------------------

(define int-mutex (make-mutex))
(define int-condvar (make-condition-variable))
(define (t) (f-tests main-disk))

;;;----------------------------------------------------
;;;                 INTERRUPT HANDLING
;;;----------------------------------------------------

(define KEYBOARD-INT #x1)
(define UART-INT #x2)
(define IDE-INT #x3)

(define INT-WITH-ARG-TABLE
  (list
    (cons KEYBOARD-INT keyboard#handle-kbd-int)
    (cons UART-INT uart#handle-uart-int)
    (cons IDE-INT ide#handle-ide-int)))

(define (handle-int int-no args)
  (let ((fn (assocv int-no INT-WITH-ARG-TABLE)))
     (apply fn args)))

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
                            ; Signal the int pump thread
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

(debug-write "AFTER INIT")
(for-each (o uart#uart-do-init ++) (iota 4))
(debug-write "DONE INIT")

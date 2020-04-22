; Mimosa
; Université de Montréal
; Marc Feeley, Samuel Yvon
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
(define SHARED-MEMORY-AREA 33554432)
(define SHARED-MEMORY-AREA-LEN 32768)

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
    (cons IDE-INT ide#handle-ide-int)
    (cons UART-INT uart#handle-uart-int)))

(define (handle-int int-no args)
  (let ((fn (assocv int-no INT-WITH-ARG-TABLE)))
     (apply fn args)))

(define (at . offset)
 (+ SHARED-MEMORY-AREA (modulo (apply + offset) SHARED-MEMORY-AREA-LEN)))

(define (erase-and-move! total-len)
  ; See the following comment for why we erase backwards, and why this is important.
  (for-each (lambda (i)
              (write-i8 #f (at total-len -1 (- i) reader-offset) #x00))
            (iota total-len))
  (set! reader-offset (modulo (+ reader-offset total-len) SHARED-MEMORY-AREA-LEN)))

(define unhandled-interrupts (open-vector))

(define (mimosa-interrupt-pump)
  ; We have no way to speak to the upper level to synchronize read operations
  ; We however need to synchronize read and write operations to the interrupt queue
  ; , since we want interrupts written in the queue to be read correctly.
  ; To achieve this, we make sure the first byte of the interrupt frame is never zero.
  ; We can ensure this by using our own interrupt codes. The C++ layer can therefore
  ; write on all memory cells that are not 0, as the first non-zero byte marks
  ; a non-empty section that follows.
  ; This is however not sufficient to ensure that accesses are done correctly.
  ; To achieve this, we need the guarantee that only one process at a time ever
  ; access the queue. Since the system is limited to a single thread/core (if not, it is now),
  ; this is not a problem.
  ; All the pump thread as to do now is ensure that bytes will never be written
  ; as zero until they have been placed into the Scheme heap. This is why we read
  ; all bytes as one go, and then erase them.
  ; To avoid the system potentially erasing interrupts that are being written
  ; while the system is switching, we also erase them starting from the end of
  ; the frame. Here is an example to see how this might happen if we do not.
  ;
  ;                      0               1   2   3   4
  ; Current frame:       | int_code  > 0 | 3 | 0 | 0 | 1 |
  ; Incoming interrupt : | int_code' > 0 | 2 | 2 | 1 |
  ; Incomplete frame:    |        0      | 0 | 0 | 0 | 1 |
  ; Partial frame :      | int_code' > 0 | 2 | 2 | 1 |
  ;
  ; If we erase from the front, we can imagine a case where cell 0 is erase up until cell
  ; 2. We can then suppose a context switch happens. We get the incomplete frame.
  ; The incoming interrupt is being written, since entries are 0, and we get the
  ; partial frame illustrated above.
  ; We context switch back to the erase routine, which completes erasing cells
  ; 2,3,4, erasing the parameters of the interrupt.
  ; This is unlikely, but it would be very, very hard to debug.
  (let pmp ()
    (let ((int-no (read-iu8 #f (at reader-offset))))
      (if (fx= 0 int-no)
          #t; stop
          (let* ((arr-len (read-iu8 #f (at 1 reader-offset)))
                 (params (map (lambda (n) (read-iu8 #f (at 2 n reader-offset))) (iota arr-len)))
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
  (mimosa-interrupt-pump)
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

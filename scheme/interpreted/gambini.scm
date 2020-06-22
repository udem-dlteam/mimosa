; Mimosa
; Université de Montréal
; Marc Feeley, Samuel Yvon
(##load-module 'keyboard)
(##load-module 'rtc)
(##load-module 'disk)
(##load-module 'fat32)
(##load-module 'ide)

(import (errors)
        (utils)
        (low-level)
        (uart)
        (debug)
        )

(define (reboot)
  (let wait-loop ()
    (let ((temp (inb #x64)))


      (if (mask temp #x01)
          (inb #x60)
          (if (mask temp #x02)
              (wait-loop)))))
  (outb #xFE #x64))

; Reader pointer for the interrupt queue
(define reader-offset 0)
; Pointer for the shared memory area
(define SHARED-MEMORY-AREA 33554432)
; Len of the shared memory area in bytes
(define SHARED-MEMORY-AREA-LEN 32768)

;;----------------------------------------------------
;;                      IMPORTs
;;----------------------------------------------------

;;----------------------------------------------------
;;                    INIT SYS
;;----------------------------------------------------

(define int-mutex (make-mutex))
(define int-condvar (make-condition-variable))

;;----------------------------------------------------
;;                 INTERRUPT HANDLING
;;----------------------------------------------------

(define INT-WITH-ARG-TABLE '())

(define (handle-int int-no args)
  (let ((int-pair (assoc int-no INT-WITH-ARG-TABLE)))
     (if int-pair
         (apply (cdr int-pair) args))))

; Compute a position from the current position
; Interrupts queue of things that need to be threated
; Offset is an array of integers that must be
; added back to the start pointer
(define (at . offset)
 (+ SHARED-MEMORY-AREA (modulo (apply + offset) SHARED-MEMORY-AREA-LEN)))

(define (erase-and-move! total-len)
  ; See the following comment for why we erase backwards, and why this is important.
  (for-each
    (lambda (i) (store-u8 #f (at total-len -1 (- i) reader-offset) #x00))
    (iota total-len))
  (set! reader-offset (modulo (+ reader-offset total-len) SHARED-MEMORY-AREA-LEN)))

; Interrupts queue of things that need to be threated
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
    (let ((int-no (fetch-u8 #f (at reader-offset))))
      (if (fx= 0 int-no)
          #t; stop
          (let* ((arr-len (fetch-u8 #f (at 1 reader-offset)))
                 (params (map (lambda (n) (fetch-u8 #f (at 2 n reader-offset))) (iota arr-len)))
                 (total-len (+ 2 arr-len)))
            (write (list int-no params) unhandled-interrupts)
            (force-output unhandled-interrupts)
            (erase-and-move! total-len) ; allow execution asap
            (pmp))
          ))))

;;----------------------------------------------------
;;                  INTERRUPT WIRING
;;----------------------------------------------------

(##interrupt-vector-set! 5 (lambda ()
                            ; Signal the int pump thread
                            ; to pump the fifo into the scheme fifo
                            (mutex-lock! int-mutex)
                            (condition-variable-signal! int-condvar)
                            (mutex-unlock! int-mutex)))

;;----------------------------------------------------
;;              INTERRUPT EXEC ROUTINE
;;----------------------------------------------------

(define (exec)
  ;; sleep if nothing
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

(thread-start! (make-thread exec 'notification-dispatcher))
(thread-start! (make-thread int-clear 'notification-pump))
(thread-start! (make-thread idle 'idle-thread))

;;----------------------------------------------------
;;                     INIT SYSTEM
;;----------------------------------------------------

; Install a driver, setup is the setup
; function for the driver. If the driver setup
; function returns a pair, it must be on the form
; (int-number . int-handling function)
(define (install-driver setup)
  (let ((r (setup)))
    (if (pair? r)
        (set! INT-WITH-ARG-TABLE (cons r INT-WITH-ARG-TABLE))
        )))

(define-macro (setup-drivers . driver-names)
 `(begin
   ,@(map (lambda (driver-name)
       `(install-driver ,(string->symbol
                           (string-append
                             (symbol->string driver-name)
                             "#"
                             (symbol->string driver-name)
                             "-setup"))))
     driver-names)))

(setup-drivers
 keyboard
 ide
 disk
 fat32
 uart
 )

;; (define fs (car fat32#filesystem-list))
;; (define main-disk (car disk#disk-list))

(define O-RDONLY #x00)
(define O-WRONLY #x01)
(define O-RDWR #x02)
(define O-CREAT 0100)
(define O-APPEND 2000)

(define file-table (make-table))

;; We ignore permissions in mimosa, so it simplifies
;; the processing
(define (mode_t->mode-string flags mode)
  (string-append
   (cond ((mask flags O-APPEND)
          "a")
         ((mask flags O-CREAT)
          "w")
         (else
          "r"
          ))
   (cond ((mask flags O-RDONLY)
          "r")
         ((mask flags O-WRONLY)
          "")
         ((mask flags O-RDWR)
          "+")
         (else
          ""))
   ))

(define (renormalize-path path)
  (define PREFIX "/dsk1")
  ;; Path used from the C Kernel use a VFS and so a dev prefix. We don't need one here
  (let* ((l (string-length path))
         (first-part (safe-substring path 0 (string-length PREFIX))))
    (if (string=? first-part PREFIX)
        (substring path (++ (string-length PREFIX)) (string-length path))
        path
        )))

;; (define (##os-device-stream-open-path path flags mode)
;;   (debug-write (mode_t->mode-string flags mode))
;;   (debug-write (renormalize-path path))
;;   (let* ((path (renormalize-path path))
;;          (mode-string (mode_t->mode-string flags mode))
;;          (f (fat32#file-open! fs path mode-string)))
;;     (if (eq? f ERR-FNF)
;;         ##err-code-ENOENT
;;         (let ((l (table-length file-table)))
;;           (table-set! file-table l f)
;;           (##os-device-stream-open-predefined l flags))
;;         )))

;; (define (##os-device-stream-read dev-condvar buffer lo hi)
;;   ;; (debug-write dev-condvar)
;;   (debug-write buffer)
;;   (debug-write lo)
;;   (debug-write hi)
;;   )

;; Test the timing of a one second wait
(define (timing-test)
  (let ((a (current-second)))
    (thread-sleep! 1)
    (let ((b (current-second)))
      (exact (round (* (- b a) 1000))))))

;; (##gc-report-set! #t)

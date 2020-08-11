; Mimosa
; Université de Montréal Marc Feeley, Samuel Yvon
(define-library (ide)
(import
  (errors)
  (gambit)
  (utils)
  (intr)
  (low-level)
  (debug))
(export
  IDE-MAX-SECTOR-READ
  handle-ide-int
  ide-read-sectors
  ide-write-sectors
  list-devices
  ide-setup
  )
(begin
  (define IDE-INT #x3)
  (define IDE-MAX-SECTOR-READ 255)
  (define IDE-DEVICE-ABSENT 'IDE-DEVICE-ABSENT)
  (define IDE-DEVICE-ATA    'IDE-DEVICE-ATA)
  (define IDE-DEVICE-ATAPI  'IDE-DEVICE-ATAPI)
  (define IDE-CONTROLLERS 4)
  (define IDE-DEVICES-PER-CONTROLLER 2)
  (define IDE-DATA-REG        0) ; 16 bit, data I/O
  (define IDE-ERROR-REG       1) ; 8 bit, error
  (define IDE-FEATURES-REG    1) ; 8 bit, features
  (define IDE-SECT-COUNT-REG  2) ; 8 bit, sector count
  (define IDE-SECT-NUM-REG    3) ; 8 bit, sector number
  (define IDE-CYL-LO-REG      4) ; 8 bit, LSB of cylinder
  (define IDE-CYL-HI-REG      5) ; 2 bit, MSB of cylinder
  (define IDE-DEV-HEAD-REG    6) ; 8 bit, 1 LBA 1 DRV HD3 HD2 HD1 HD0
  (define IDE-STATUS-REG      7)
  (define IDE-ALT-STATUS-REG  #x206)
  (define IDE-COMMAND-REG     7)
  (define IDE-DEV-CTRL-REG    #x206)
  (define IDE-DRIVE-ADDR-REG  #x207)
  (define IDE-STATUS-BSY (expt 2 7))  ; Device busy bit
  (define IDE-STATUS-RDY (expt 2 6))  ; Device ready bit
  (define IDE-STATUS-DF (expt 2 5))   ; Device fault bit
  (define IDE-STATUS-DSC (expt 2 4))  ; Drive seek complete bit
  (define IDE-STATUS-DRQ (expt 2 3))  ; Data request bit
  (define IDE-STATUS-CORR (expt 2 2))  ; Corrected data bit
  (define IDE-STATUS-INDEX (expt 2 1))  ; Index bit
  (define IDE-STATUS-ERR 1)    ; Error bit
  (define IDE-ERROR-BBK   (expt 2 7)) ; Bad block mark detected in sector's ID field
  (define IDE-ERROR-UNC   (expt 2 6)) ; Uncorrectable data error encountered
  (define IDE-ERROR-IDNF  (expt 2 4)) ; Requested sector's ID field not found
  (define IDE-ERROR-ABRT  (expt 2 2)) ; Command aborted (status error or invalid cmd)
  (define IDE-ERROR-TK0NF (expt 2 1)) ; Track 0 not found during recalibrate command
  (define IDE-ERROR-AMNF  (expt 2 0)) ; Data address mark not found after ID field
  (define IDE-DEV-CTRL-SRST (expt 2 2)) ; Software reset bit
  (define IDE-DEV-CTRL-nIEN (expt 2 1)) ; Interrupt enable bit (0=enabled)
  (define IDE-DEV-HEAD-IBM #xa0)
  (define IDE-DEV-HEAD-LBA (fxior (expt 2 6) IDE-DEV-HEAD-IBM)) ; LBA address
  (define (IDE-DEV-HEAD-DEV x) (* x (expt 2 4))) ; Device index (0 or 1)
  (define IDE-EXEC-DEVICE-DIAG-CMD       #x90)
  (define IDE-FLUSH-CACHE-CMD            #xe7)
  (define IDE-IDENTIFY-DEVICE-CMD        #xec)
  (define IDE-IDENTIFY-PACKET-DEVICE-CMD #xa1)
  (define IDE-IDLE-CMD                   #xe3)
  (define IDE-IDLE-IMMEDIATE-CMD         #xe1)
  (define IDE-MEDIA-EJECT-CMD            #xed)
  (define IDE-MEDIA-LOCK-CMD             #xde)
  (define IDE-MEDIA-UNLOCK-CMD           #xdf)
  (define IDE-NOP-CMD                    #x00)
  (define IDE-READ-DMA-CMD               #xc8)
  (define IDE-READ-DMA-QUEUED-CMD        #xc7)
  (define IDE-READ-MULTIPLE-CMD          #xc4)
  (define IDE-READ-SECTORS-CMD           #x20)
  (define IDE-SEEK-CMD                   #x70)
  (define IDE-SET-FEATURES-CMD           #xef)
  (define IDE-WRITE-DMA-CMD              #xca)
  (define IDE-WRITE-DMA-QUEUED-CMD       #xcc)
  (define IDE-WRITE-MULTIPLE-CMD         #xc5)
  (define IDE-WRITE-SECTORS-CMD          #x30)
  (define IDE-LOG2-SECTOR-SIZE 9)
  (define MAX-NB-IDE-CMD-QUEUE-ENTRIES 1)
  (define IDE-CTRL-0 #x1f0)
  (define IDE-IRQ-0 14)
  (define IDE-CTRL-1 #x170)
  (define IDE-IRQ-1 15)
  (define IDE-CTRL-2 #x1e8)
  (define IDE-IRQ-2 12)
  (define IDE-CTRL-3 #x168)
  (define IDE-IRQ-3 10)

  (define LOG-FILE 'NO-FILE)

  (define (log message)
    (if (eq? LOG-FILE 'NO-FILE)
        (debug-write "LOG Failure : No log file")
        (write-string (string-append (debug-write message) "\n") LOG-FILE)))

  (define IDE-CONTROLLER-PORTS
    (list
      IDE-CTRL-0
      IDE-CTRL-1
      IDE-CTRL-2
      IDE-CTRL-3))

  (define IDE-CONTROLLER-IRQS
    (list
      IDE-IRQ-0
      IDE-IRQ-1
      IDE-IRQ-2
      IDE-IRQ-3))

  ; Verify that the status indicates that the disk
  ; absent
  (define (not-absent? status)
    (let ((mask (fxior
                  IDE-STATUS-BSY
                  IDE-STATUS-RDY
                  IDE-STATUS-DF
                  IDE-STATUS-DSC
                  IDE-STATUS-DRQ)))
      (not (fx= (fxand status mask) mask))))

  (define-type ide-device
               id ; the ID of the device
               kind ; the kind of device it is
               controller ; the controller of the device
               serial ; the serial number of the device
               firmware-rev ; the firmware revision of the device
               model-num ; the model number of the device
               cylinders-per-disks ; the number of cylinders per disk of the device
               heads-per-cylinder ; self explanatory
               sectors-per-track ; the number of sectors per track
               total-sectors-chs ; total sector in C/H/S notation
               total-sectors ; the total number of sectors
               purpose ; either the symbol HARD-DISK or REMOVABLE

               )

  (define-type ide-controller
               controller-id ; the id of the controller
               cpu-port ; the cpu port of the IDE controller
               irq ; IRQ associated with the controller
               continuations-queue ; queue of continuations that needs to be executed
               mut; condvar mutex
               cv ; condvar itself
               )

  ; Handle an IDE read error with the CPU port of the controller as a way
  ; to identify the port
  (define (ide-handle-read-err cpu-port)
    (let* ((err-reg (fx+ cpu-port IDE-ERROR-REG))
           (error (inb err-reg)))
      (if (mask error IDE-ERROR-BBK)
          (debug-write "Bad block mark detected in sector's ID field"))

      (if (mask error IDE-ERROR-UNC)
          (debug-write "Uncorrectable data error encountered"))

      (if (mask error IDE-ERROR-IDNF)
          (debug-write "Requested sector's ID field not found"))

      (if (mask error IDE-ERROR-ABRT)
          (debug-write "Command aborted (status error or invalid command)"))

      (if (mask error IDE-ERROR-TK0NF)
          (debug-write "Track 0 not found during recalibrate command"))

      (if (mask error IDE-ERROR-AMNF)
          (debug-write "Data address mark not found after ID field")))

    ERR-ARGS)

  ; Take a vector made of shorts (16 bit wide ints) and return a vector made
  ; of bytes (system endianness)
  (define (expand-wvect wvector)
    (let ((get-byte (lambda (i)
                      (let* ((even (fxeven? i))
                             (half-pos (if even
                                           (fxhalve i)
                                           (fxhalve (- i 1))))
                             (val (vector-ref wvector half-pos)))
                        (b-chop (if (not even) (>> val 8) val))))))
      (build-vector (* 2 (vector-length wvector)) get-byte)))

  ; Take a vector of bytes and compresses it into a vector
  ; of shorts (16 bit wide ints)
  (define (compress-bvect b-vect)
    (let ((l (vector-length b-vect)))
      (build-vector
        (fxhalve l)
        (lambda (idx)
          (let ((b-idx (<< idx 1)))
            (fxior
              (<< (vector-ref b-vect (+ b-idx 1)) 8)
              (vector-ref b-vect b-idx)))))))

  ; Flush the command cache of an ide device
  ; c: the success continuation, nothing in it
  ; e: the error continuation, with the error code
  (define (ide-flush-cache device c e)
    (let* ((dev-id (ide-device-id device))
           (ctrl ((ide-device-controller device)))
           (cpu-port (ide-controller-cpu-port ctrl))
           (q (ide-controller-continuations-queue ctrl))
           (mut (ide-controller-mut ctrl))
           (cv (ide-controller-cv ctrl))
           (stt-reg (fx+ cpu-port IDE-STATUS-REG))
           (err #f)
           (cmd-reg (fx+ cpu-port IDE-COMMAND-REG)))
      (mutex-lock! mut)
      (write
        (lambda ()
          (if (mask (inb stt-reg) IDE-STATUS-ERR)
              (set! err ERR-HWD))
          (mutex-lock! mut)
          (condition-variable-signal! cv)
          (mutex-unlock! mut))
        q)
      (force-output q)
      (outb IDE-FLUSH-CACHE-CMD cmd-reg)
      (mutex-unlock! mut cv)
      (if err (e err) (c))))

  ; Read `count` sectors from the ide device.
  ; When the data is read, the continuation is called with
  ; a vector that corresponds to the data at `lba` (logical block addressing)
  ; c is a continuation that takes the vector as parameter
  ; e is an error continuation, taking an error symbol as a parameter
  (define (ide-read-sectors device lba count c e)
    (if (fx> count 0)
        (let* ((dev-id (ide-device-id device))
               (ctrl ((ide-device-controller device)))
               (mut (ide-controller-mut ctrl))
               (cv (ide-controller-cv ctrl))
               (cpu-port (ide-controller-cpu-port ctrl))
               (head-reg (fx+ cpu-port IDE-DEV-HEAD-REG))
               (sect-count-reg (fx+ cpu-port IDE-SECT-COUNT-REG))
               (sect-num-reg (fx+ cpu-port IDE-SECT-NUM-REG))
               (cyl-lo-reg (fx+ cpu-port IDE-CYL-LO-REG))
               (cyl-hi-reg (fx+ cpu-port IDE-CYL-HI-REG))
               (data-reg (fx+ cpu-port IDE-DATA-REG))
               (alt-reg (fx+ cpu-port IDE-ALT-STATUS-REG))
               (cmd-reg (fx+ cpu-port IDE-COMMAND-REG))
               (stt-reg (fx+ cpu-port IDE-STATUS-REG))
               (q (ide-controller-continuations-queue ctrl))
               (count (min 256 count))
               (err #f)
               (sz (<< count (- IDE-LOG2-SECTOR-SIZE 1)))
               (word-vector (make-vector sz 0)))
          (mutex-lock! mut)
          (write
            (lambda ()
              (mutex-lock! mut)
              (let* ((status (inb stt-reg)))
                (if (mask status IDE-STATUS-ERR)
                    (set! err (ide-handle-read-err cpu-port))
                    (begin
                      (for-each
                        (lambda (i) (vector-set! word-vector i (inw data-reg)))
                        (iota sz))
                      ; TODO: figure out why one more sector avail
                      (if (mask IDE-STATUS-DRQ (inb alt-reg))
                          (set! err ERR-HWD))))
                ; Signal we are ready
                (condition-variable-signal! cv)
                (mutex-unlock! mut)))
            q)
          (force-output q)
          (outb
            (b-chop (fxior
                      IDE-DEV-HEAD-LBA
                      (IDE-DEV-HEAD-DEV dev-id)
                      (>> lba 24)))
            head-reg)
          (outb (b-chop count) sect-count-reg)
          (outb (b-chop lba) sect-num-reg)
          (outb (b-chop (>> lba 8)) cyl-lo-reg)
          (outb (b-chop (>> lba 16)) cyl-hi-reg)
          (outb (b-chop IDE-READ-SECTORS-CMD) cmd-reg)
          (mutex-unlock! mut cv)
          (if err
              (e err)
              (c (expand-wvect word-vector))))
        (e ERR-ARGS)))

  ; Check if the device is absent
  (define (device-absent? dev)
    (eq? dev IDE-DEVICE-ABSENT))

  (define (ide-write-sectors device lba buffer count c e)
    (let* ((count (min count 256))
           (compressed-vector (compress-bvect buffer))
           (dev-id (ide-device-id device))
           (ctrl ((ide-device-controller device)))
           (cpu-port (ide-controller-cpu-port ctrl))
           (data-reg (fx+ cpu-port IDE-DATA-REG))
           (head-reg (fx+ cpu-port IDE-DEV-HEAD-REG))
           (sect-count-reg (fx+ cpu-port IDE-SECT-COUNT-REG))
           (sect-num-reg (fx+ cpu-port IDE-SECT-NUM-REG))
           (cyl-lo-reg (fx+ cpu-port IDE-CYL-LO-REG))
           (cyl-hi-reg (fx+ cpu-port IDE-CYL-HI-REG))
           (cmd-reg (fx+ cpu-port IDE-COMMAND-REG))
           (stt-reg (fx+ cpu-port IDE-STATUS-REG))
           (mut (ide-controller-mut ctrl))
           (cv (ide-controller-cv ctrl))
           (err #f)
           (q (ide-controller-continuations-queue ctrl))
           (count (min 256 count)))
      (mutex-lock! mut)
      (write
        (lambda ()
          (mutex-lock! mut)
          (if (mask (inb stt-reg) IDE-STATUS-ERR)
              (begin
                (debug-write "Failed to write to disk...")
                (set! err ERR-HWD)))
          (condition-variable-signal! cv)
          (mutex-unlock! mut))
        q) ; nothing really to do, maybe add a signal later?
      (force-output q)
      (outb
        (b-chop
          (fxior
            IDE-DEV-HEAD-LBA
            (IDE-DEV-HEAD-DEV dev-id)
            (>> lba 24)))
        head-reg)
      (outb (b-chop count) sect-count-reg)
      (outb (b-chop lba) sect-num-reg)
      (outb (b-chop (>> lba 8)) cyl-lo-reg)
      (outb (b-chop (>> lba 16)) cyl-hi-reg)
      (outb (b-chop IDE-WRITE-SECTORS-CMD) cmd-reg)
      ; Wait until the disk is ready
      ; There should be an interrupt sent but somehow
      ; it never comes for the first sector
      (let spin-loop ()
        (if (or (mask (inb stt-reg) IDE-STATUS-BSY)
                (not (mask (inb stt-reg) IDE-STATUS-DRQ)))
            (begin
              (ide-delay cpu-port)
              (spin-loop))))
      (for-each
        (lambda (i) (outw (vector-ref compressed-vector i) data-reg))
        (iota (<< 1 (- IDE-LOG2-SECTOR-SIZE 1))))
      (mutex-unlock! mut cv) ; wait until the IRQ was received
      (if err
          (e err)
          ; flush to confirm no more writes are going to occur
          (ide-flush-cache device c e))))

  ; Init the devices struct
  (define (ide-init-controllers)
    (list->vector
      (map
        (lambda (i)
          (make-ide-controller
            i ; the id
            (list-ref IDE-CONTROLLER-PORTS i) ; the cpu-port
            (list-ref IDE-CONTROLLER-IRQS i) ; the irq
            (open-vector) ; make a 10 item queue
            (make-mutex)
            (make-condition-variable)))
        (iota IDE-CONTROLLERS))))

  ; Vectors of IDE controllers, it contains the controller present on the
  ; system.
  (define IDE-CTRL-VECT (ide-init-controllers))

  (define (ide-delay cpu-port)
    ; We read the alternative status reg.
    ; it doesnt erase it, and is the recommanded way of
    ; waiting on an ide device
    (for-each (lambda (n) (inb (fx+ cpu-port IDE-ALT-STATUS-REG))) (iota 4)))

  (define (swap-and-trim vect offset len)
    (let* ((idcs (iota len))
           (extract-char
             (lambda (idx)
               (fxand ;; integer are not fixed-width in Scheme,
                 #xFF ;; collapse it unto a byte
                 (if (mask idx 1)
                     (vector-ref vect (+ offset (>> idx 1)))
                     (>> (vector-ref vect (+ offset (>> idx 1))) 8)))))
           (untrimmed (list->string (map (o integer->char extract-char) idcs))))
      (string-trim untrimmed)))

  ; Dispatches IDE interrupts. It takes the continuation queue for a controller
  ; and simply calls the first one
  (define (handle-ide-int controller-no)
    (let* ((ctrl (vector-ref IDE-CTRL-VECT controller-no))
           (q (ide-controller-continuations-queue ctrl))
           (cont (read q)))
      (ack-irq (if (= controller-no 0)
                   14
                   15))
      (call-if cont)))

  ; Switch the IDE management from the C kernel to the scheme kernel
  (define (switch-over-driver)
    (open-input-file "/cut"))

  ; Create a list of ide devices
  (define (list-devices)
    (filter (flatten
              (vector->list (vector-map ide-controller-devices IDE-CTRL-VECT)))
            (o not device-absent?)))

  (define (make-head-command lba? master? head)
    (fxior
      (<< 1 7) ;; 1
      (<< (if lba? 1 0) 6) ;; LBA?
      (<< 1 5) ;; 1
      (<< (if master? 0 1) 4) ;; DRV
      (<< (if (mask head 8) 1 0) 3) ;; HEAD NO MSb
      (<< (if (mask head 4) 1 0) 2)
      (<< (if (mask head 2) 1 0) 1)
      (<< (if (mask head 1) 1 0) 0) ;; HEAD NO LSb
      ))

  (define (master?-to-str master?) (if master?  "MASTER" "SLAVE"))

  ; Vectors of IDE controllers, it contains the controller present on the
  ; system.
  (define IDE-CTRL-VECT (ide-init-controllers))

  (define device-vector (vector 0 0 0 0 0 0 0 0))

  (define (device-vector-get cont-num master?)
    (let ((offset (if master? 0 1)))
      (vector-ref device-vector (+ (* 2 cont-num) offset))
      ))

  (define (device-vector-set! cont-num master? v)
    (let ((offset (if master? 0 1)))
      (vector-set! device-vector (+ (* 2 cont-num) offset) v)
      ))

  ;; Allow a boolean function to be
  ;; retried nth time before being
  ;; given up
  (define (with-retry nth predicate)
    (if (= nth 0)
        #f
        (let ((r (predicate)))
          (if r
              #t
              (begin
                (thread-sleep! (until-has-elapsed 1 TIME-UNIT-MICROSECS))
                (with-retry (- nth 1) predicate))))))

  ;; Setup a device for the controller.
  ;; It will read the device information
  ;; and setup the cache
  (define (setup-device cont master?)
    (let* ((id (ide-controller-controller-id cont))
           (cpu-port (ide-controller-cpu-port cont))
           (head-reg (fx+ cpu-port IDE-DEV-HEAD-REG))
           (status-reg (fx+ cpu-port IDE-STATUS-REG))
           (sector-reg (fx+ cpu-port IDE-SECT-COUNT-REG))
           (cy-lo-reg (fx+ cpu-port IDE-CYL-LO-REG))
           (cy-hi-reg (fx+ cpu-port IDE-CYL-HI-REG))
           (data-reg (fx+ cpu-port IDE-DATA-REG))
           (err-reg (fx+ cpu-port IDE-ERROR-REG))
           (stt-reg (fx+ cpu-port IDE-STATUS-REG))
           (cmd-reg (fx+ cpu-port IDE-COMMAND-REG)))
      (log (string-append
          "Setting up device "
          (master?-to-str master?)
          " of controller "
          (number->string id)))
      (if (with-retry 300 (partial detect-device cont master?))
          (begin
            ;; Device is here...
            ;; We read the magic signature to see if the drives
            ;; are ATA or ATAPI. This OSDEV articles describes it well:
            ;; https://wiki.osdev.org/ATAPI
            ;; Controller just reset the drives, so they should have their
            ;; signature.
            (outb (make-head-command #f master? 0) head-reg) ;; Select the drive
            (let* ((lo (inb cy-lo-reg))
                   (hi (inb cy-hi-reg))
                   (type (if (and (fx= #x14 lo) ;; Magic signature
                                  (fx= #xeb hi))
                             IDE-DEVICE-ATAPI
                             IDE-DEVICE-ATA))
                   ;; Last presence check... Status should not be 0 unless ATA-PI
                   (present (or (eq? type IDE-DEVICE-ATAPI)
                                (not (fx= (inb stt-reg) 0))
                                )))
              (if present
                  (begin
                    (outb (make-head-command #f master? 0) head-reg) ;; Select again
                    (outb (if (eq? type IDE-DEVICE-ATA)
                              IDE-IDENTIFY-DEVICE-CMD
                              IDE-IDENTIFY-PACKET-DEVICE-CMD)
                          cmd-reg)
                    (log (string-append  "Device is of type: " (symbol->string type)))
                    ;; Interrupts are disabled, we have to poll
                    ;; We can also read the status reg without fear
                    (if (with-retry 500 (lambda ()
                                          (let ((status (inb stt-reg)))
                                            (not (mask status IDE-STATUS-BSY)))))
                        ;; The device is here. The ID packet is one sector long, but made
                        ;; out of words (of 16 bytes), so we read 256 of them
                        (begin
                          (let* ((size (<< 1 (- IDE-LOG2-SECTOR-SIZE 1)))
                                 ;; The result of the identify device command is detailed
                                 ;; in The Indispensible PC Hardware book.
                                 (id-vect (build-vector size (lambda _ (inw data-reg))))
                                 (configuration (vector-ref id-vect 0))
                                 (serial-num (swap-and-trim id-vect 10 20))
                                 (firmware-rev (swap-and-trim id-vect 23 8))
                                 (model-num (swap-and-trim id-vect 27 40))
                                 ; If has extended options
                                 (has-extended
                                   (mask (vector-ref id-vect 53) 1))
                                 (cyl-per-dsk
                                   (vector-ref id-vect (if has-extended 54 1)))
                                 (heads-per-cyl
                                   (vector-ref id-vect (if has-extended 55 3)))
                                 (sect-per-trk
                                   (vector-ref id-vect (if has-extended 56 6)))
                                 (total-sectors
                                   (fx+ (<< (vector-ref id-vect 61) 16) (vector-ref id-vect 60)))
                                 (total-sectors-chs
                                   (if has-extended
                                       (fx+
                                         (<< (vector-ref id-vect 58) 16)
                                         (vector-ref id-vect 57)) 0))
                                 (device (make-ide-device
                                           master?
                                           type
                                           (lazy cont)
                                           serial-num
                                           firmware-rev
                                           model-num
                                           cyl-per-dsk
                                           heads-per-cyl
                                           sect-per-trk
                                           total-sectors-chs
                                           total-sectors
                                           (let ((hard-disk-bit (mask configuration (<< 1 6)))
                                                 (removable-bit (mask configuration (<< 1 7))))
                                            (cond ((and hard-disk-bit removable-bit) 'BOTH)
                                                  (hard-disk-bit 'HARD-DISK)
                                                  (removable-bit 'REMOVABLE)
                                                  (else 'UNKNOWN)))
                                           )))
                            (log (string-append "Device " (master?-to-str master?) " is here"))
                            (if (not (eq? (ide-device-purpose device) 'UNKNOWN))
                                (device-vector-set! id master? device))
                            ))))))))))

  ;; Detect an IDE device on a controller
  ;; It reads the status register
  ;; and detects if a device is present.
  (define (detect-device cont master?)
    (let* ((id (ide-controller-controller-id cont))
           (cpu-port (ide-controller-cpu-port cont))
           (head-reg (fx+ cpu-port IDE-DEV-HEAD-REG))
           (status-reg (fx+ cpu-port IDE-STATUS-REG))
           (err-reg (fx+ cpu-port IDE-ERROR-REG))
           (cmd-reg (fx+ cpu-port IDE-COMMAND-REG)))
      (outb (make-head-command #f master? 0) head-reg) ;; Set the disk
      (ide-delay cpu-port) ;; wait...
      (let* ((status (inb status-reg))) ;; check the status
        (not-absent? status))))

  ;; More sophisticated method to detect the hardware,
  ;; This does not seem to be documented anywhere but
  ;; the current code base. We basically perform the same check
  ;; but if the device is busy, we assume it is not a real device
  ;; This needs to be tried in a loop
  (define (advanced-detect-device cont master?)
    (let* ((id (ide-controller-controller-id cont))
           (cpu-port (ide-controller-cpu-port cont))
           (head-reg (fx+ cpu-port IDE-DEV-HEAD-REG))
           (status-reg (fx+ cpu-port IDE-STATUS-REG))
           (err-reg (fx+ cpu-port IDE-ERROR-REG))
           (cmd-reg (fx+ cpu-port IDE-COMMAND-REG)))
      (with-retry
        30000
        (lambda ()
          ;; Set the disk
          (outb (make-head-command #f master? 0) head-reg)
          (ide-delay cpu-port)
          (let* ((status (inb status-reg))) ;; check the status
           ;; Device should NOT be busy
           (fx= 0 (fxand status IDE-STATUS-BSY)))))
      ))

  (define (setup-controller cont)
    (log (string-append "Setting up controller " (number->string (ide-controller-controller-id cont))))
    (let* ((cpu-port (ide-controller-cpu-port cont))
           (irq (ide-controller-irq cont))
           (head-reg (fx+ cpu-port IDE-DEV-HEAD-REG))
           (ctrl-reg (fx+ cpu-port IDE-DEV-CTRL-REG))
           (stt-reg (fx+ cpu-port IDE-STATUS-REG))
           (err-reg (fx+ cpu-port IDE-ERROR-REG))
           (cyl-lo-reg (fx+ cpu-port IDE-CYL-LO-REG))
           (cyl-hi-reg (fx+ cpu-port IDE-CYL-HI-REG))
           (short-sleep (lambda () (until-has-elapsed 5 TIME-UNIT-MICROSECS))))
      (if (or (detect-device cont #t)
              (detect-device cont #f))
          (begin
            (outb IDE-DEV-CTRL-nIEN ctrl-reg) ;; disable interrupts
            (thread-sleep! (short-sleep))
            (outb
              (fxior IDE-DEV-CTRL-nIEN IDE-DEV-CTRL-SRST)
              ctrl-reg) ;; with interrupts disabled, reset the drives
            (thread-sleep! (short-sleep))
            (outb IDE-DEV-CTRL-nIEN ctrl-reg) ;; keep disabled interrupts, resume drive
            (if (advanced-detect-device cont #t)
                (setup-device cont #t))
            (if (advanced-detect-device cont #f)
                (setup-device cont #f))
            (outb #x00 ctrl-reg) ;; reenable everything
            ))))

  (define (ide-setup)
    (set! LOG-FILE (open-file (list path: "ide.txt" append: #t)))
    (log "IDE")
    (log "Testing the new setup...")
    (for-each setup-controller (vector->list IDE-CTRL-VECT))
    (log "IDE Setup done")
    (switch-over-driver)
    (cons IDE-INT handle-ide-int)) ;; return value expected by the setup routine
  ))

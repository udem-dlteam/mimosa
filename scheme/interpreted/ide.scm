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
      (not (fx= (fxand status mask) mask))
      ))

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
               )

  (define-type ide-controller
               controller-id ; the id of the controller
               cpu-port ; the cpu port of the IDE controller
               irq ; IRQ associated with the controller
               devices ; list of devices
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

  ; make a list of devices for a controller
  (define (ide-init-devices)
    (make-list
      IDE-DEVICES-PER-CONTROLLER
      IDE-DEVICE-ABSENT))

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
            (ide-init-devices)
            (open-vector) ; make a 10 item queue
            (make-mutex)
            (make-condition-variable)))
        (iota IDE-CONTROLLERS))))

  ; Set the device of the ide controller to the specified device
  (define (ide-controller-set-device controller dev-no device)
    (let* ((dev-list (ide-controller-devices controller)))
      (list-set! dev-list dev-no device)))

  ; Execute `cont` on the device `dev-no` of the controller
  ; if there is such a device. `cont` must be a lambda that
  ; accepts the device has a first parameter
  (define (ide-controller-if-device? controller dev-no cont)
    (let* ((ctrls-devices (ide-controller-devices controller))
           (wanted-device (list-ref ctrls-devices dev-no)))
      (if (not (symbol? wanted-device))
          (cont wanted-device)
          #f)))

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

  (define (setup-device controller devices dev-no)
    (let* ((cpu-port (ide-controller-cpu-port controller))
           (head-reg (fx+ cpu-port IDE-DEV-HEAD-REG))
           (cmd-reg (fx+ cpu-port IDE-COMMAND-REG))
           (stt-reg (fx+ cpu-port IDE-STATUS-REG))
           (data-reg (fx+ cpu-port IDE-DATA-REG))
           (device-type (list-ref devices dev-no))
           (err 0))
      (if (not (eq? device-type IDE-DEVICE-ABSENT))
          (begin
            ; Identify device packet
            (outb (fxior IDE-DEV-HEAD-IBM (IDE-DEV-HEAD-DEV dev-no)) head-reg)
            (outb (if (eq? device-type IDE-DEVICE-ATA)
                      IDE-IDENTIFY-DEVICE-CMD
                      IDE-IDENTIFY-PACKET-DEVICE-CMD) cmd-reg)
            (let wait-loop ((j 0))
              (let ((status (inb stt-reg)))
                (if (not (mask status IDE-STATUS-BSY))
                    (if (mask status IDE-STATUS-ERR)
                        (set! err 1))
                    (begin
                      (thread-sleep! (until-has-elapsed 1 TIME-UNIT-MICROSECS))
                      (wait-loop (+ j 1))))))
            ; Device is not absent, we can continue the work
            (if (> err 0)
                (list-set! devices dev-no IDE-DEVICE-ABSENT)
                (let* ((info-sz (<< 1 (- IDE-LOG2-SECTOR-SIZE 1)))
                       (id-vect (build-vector info-sz (lambda (idx) (inw data-reg))))
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
                                 dev-no
                                 (list-ref devices dev-no)
                                 (lazy controller)
                                 serial-num
                                 firmware-rev
                                 model-num
                                 cyl-per-dsk
                                 heads-per-cyl
                                 sect-per-trk
                                 total-sectors-chs
                                 total-sectors)))
                  (log (string-append "Installing device "
                        (number->string dev-no)
                        " : "
                        (symbol->string (list-ref devices dev-no))
                        " : "
                        model-num
                        ))
                  (ide-controller-set-device controller dev-no device)))))))

  ; Make a lambda to detect if a device is present on the ide
  ; controller whoses CPU port is the cpu-port in parameters
  (define (detect-device controller controller-statuses cpu-port dev-no)
    (log (string-append "Detecting device at: " (number->string controller) ":" (number->string cpu-port) ":" (number->string dev-no)))
    (let* ((device-reg (fx+ cpu-port IDE-DEV-HEAD-REG)))
      (outb (fxior IDE-DEV-HEAD-IBM (IDE-DEV-HEAD-DEV dev-no)) device-reg)
      ; Send detection info
      (ide-delay cpu-port)
      (let* ((status (inb (fx+ cpu-port IDE-STATUS-REG))))
        (vector-set! controller-statuses dev-no status)
        (log (string-append "Status is: " (number->string status)))
        (log (string-append "Absent?: " (if (not (not-absent? status))
                                         "yes"
                                         "no")))
        (if (not-absent? status)
            IDE-DEVICE-ATAPI
            IDE-DEVICE-ABSENT))))

  ; Reset an ide device
  ; type: the type of the device
  ; ctrl-cpu-port: the cpu port the cpu port where to write out (of the ctrler)
  ; device-no: the device number
  ; returns true if the device was successfully reset, false otherwise
  (define (ide-reset-device type ctrl-cpu-port device-no)
    (log
      (string-append
        "[ide-reset-device] Resetting device, "
        (symbol->string type)
        ":"
        (number->string ctrl-cpu-port)
        ":"
        (number->string device-no)))
    (let ((head-reg (fx+ ctrl-cpu-port IDE-DEV-HEAD-REG))
          (stt-reg (fx+ ctrl-cpu-port IDE-STATUS-REG)))
      (outb (fxior IDE-DEV-HEAD-IBM (IDE-DEV-HEAD-DEV device-no)) head-reg)
      (ide-delay ctrl-cpu-port)
      (let ((r (and
                 (fx= (fxand (inb stt-reg) IDE-STATUS-BSY) 0)
                 (eq? type IDE-DEVICE-ATAPI))))
       (log (if r
             "Reset OK"
             "Reset failed"))
       r)))

  ; Confirm the devices that are really connected to the controller
  ; Takes a controller, a list of the candidate devices and the number
  ; of candidates, and return an updated number of candidates
  (define (confirm-devices controller devices candidates)
    (log
     (string-append
       "[confirm-devices] Confirming the presence of devices for "
       (number->string (ide-controller-controller-id controller))
       ", "
       (number->string candidates)
       " (candidates)"))
    (let ((cpu-port (ide-controller-cpu-port controller)))
      (let wait-loop
        ((j 0)
         (break #f))
        (if (and (not break)
                 (< j 30000))
            (begin
              (for-each
                (lambda (device-no)
                  (let ((dev-type (list-ref devices device-no)))
                    ; add IDE device type
                    (if (ide-reset-device dev-type cpu-port device-no)
                        (begin
                          (set! candidates (- candidates 1))
                          (list-set! devices device-no IDE-DEVICE-ATA)))))
                (iota IDE-DEVICES-PER-CONTROLLER))
              (thread-sleep! (until-has-elapsed 1 TIME-UNIT-MS))
              (wait-loop
                (++ j)
                (<= candidates 0)))
            candidates))))

  ; Reset an IDE controller. Takes for input a controller, the devices
  ; for the controller and the candidate devicess
  (define (reset-controller controller devices statuses candidates)
    (log (string-append "[reset-controller] Resetting controller " (number->string (ide-controller-controller-id controller))))
    (let* ((cpu-port (ide-controller-cpu-port controller))
           (head-reg (fx+ cpu-port IDE-DEV-HEAD-REG))
           (ctrl-reg (fx+ cpu-port IDE-DEV-CTRL-REG))
           (stt-reg (fx+ cpu-port IDE-STATUS-REG))
           (err-reg (fx+ cpu-port IDE-ERROR-REG))
           (cyl-lo-reg (fx+ cpu-port IDE-CYL-LO-REG))
           (cyl-hi-reg (fx+ cpu-port IDE-CYL-HI-REG))
           (short-sleep (lambda () (until-has-elapsed 5 TIME-UNIT-MICROSECS))))
      (outb (fxior IDE-DEV-HEAD-IBM (IDE-DEV-HEAD-DEV 0)) head-reg)
      (ide-delay cpu-port)
      (inb stt-reg)
      (thread-sleep! (short-sleep))
      (outb IDE-DEV-CTRL-nIEN ctrl-reg)
      (thread-sleep! (short-sleep))
      (outb (fxior IDE-DEV-CTRL-nIEN IDE-DEV-CTRL-SRST) ctrl-reg)
      (thread-sleep! (short-sleep))
      (outb IDE-DEV-CTRL-nIEN ctrl-reg)
      (thread-sleep! (until-has-elapsed 1 TIME-UNIT-MS))
      (inb err-reg)
      (thread-sleep! (short-sleep))
      (set! candidates (confirm-devices controller devices candidates))
      (if (> (apply + (map (lambda (dev) (if (eq? dev IDE-DEVICE-ATA) 1 0)) devices)) 0)
          ; There are candidates
          (begin
            (log (string-append "Candidates found for controller" (number->string (ide-controller-controller-id controller))))
            (for-each (lambda (dev)
                        (outb (fxior IDE-DEV-HEAD-IBM (IDE-DEV-HEAD-DEV dev)) head-reg)
                        (ide-delay cpu-port)
                        (if (and (fx= (inb cyl-lo-reg) #x14)
                                 (fx= (inb cyl-hi-reg) #xeb)
                                 (eq? (list-ref devices dev) IDE-DEVICE-ATA))
                            ; Update the list: this is an ATAPI device
                            (begin
                             (log (string-append "ATAPI device: " (number->string dev)))
                             (list-set! devices dev IDE-DEVICE-ATAPI))
                            (log (string-append "Not an ATAPI device: " (number->string dev)))))
                      (iota IDE-DEVICES-PER-CONTROLLER))
            (for-each (lambda (dev)
                        (let ((dev-type (list-ref devices dev)))
                          (log (string-append "Inspect device " (number->string dev) ": " (symbol->string dev-type)))
                          (if (eq? IDE-DEVICE-ATA dev-type)
                              (begin
                                (log (string-append "Reading the status of the device" (number->string dev)))
                                (if (fx= 0 (vector-ref statuses dev))
                                    ; A zero status ATA is absent
                                    (begin
                                     (log "Zero status: absent")
                                     (list-set! devices dev IDE-DEVICE-ABSENT))
                                    ; Make sure the device is present
                                    (begin
                                      (log "Non-zero status: present")
                                      (outb (fxior IDE-DEV-HEAD-IBM (IDE-DEV-HEAD-DEV dev)) head-reg)
                                      (ide-delay cpu-port)
                                      ; TODO: remove these magic numbers
                                      (outb #x58 err-reg)
                                      (outb #xA5 cyl-lo-reg)
                                      (if (or (fx= #x58 (inb err-reg))
                                              (not (fx= #xA5 (inb cyl-lo-reg))))
                                          (list-set! devices dev IDE-DEVICE-ABSENT))
                                      ))))))
                      (iota IDE-DEVICES-PER-CONTROLLER))))
      ; Setup command queue
      (let ((setup-device (partial setup-device controller devices)))
        (for-each setup-device (iota IDE-DEVICES-PER-CONTROLLER)))
      ; Enable ints on the controller
      (outb 0 ctrl-reg)))

  ; Setup an ide controller
  (define (setup-controller no)
    (log (string-append "Setting up controller " (number->string no)))
    (let* ((controller (vector-ref IDE-CTRL-VECT no))
           (cpu-port (ide-controller-cpu-port controller))
           (irq (ide-controller-irq controller)))
      (let* ((statuses (make-vector IDE-DEVICES-PER-CONTROLLER 0))
             ; Detection function for the devices
             (detect (partial detect-device no statuses cpu-port))
             ; List of devices detected
             (devices (map detect (iota IDE-DEVICES-PER-CONTROLLER)))
             ; Number of devices that might be there (might turn out to not be later on)
             (candidates (apply + (map (lambda (device)
                                         (if (eq? device IDE-DEVICE-ABSENT) 0 1))
                                       devices))))
        ; (for-each (lambda (candidate)
        ;  (debug-write candidate))
        ;  candidates)
        (log (string-append "Has found " (number->string candidates) " candidates"))
        (if (> candidates 0)
            (reset-controller controller devices statuses candidates)
            #f))))

  ; Switch the IDE management from the C kernel to the scheme kernel
  (define (switch-over-driver)
    (open-input-file "/cut"))

  ; Setup the IDE driver
  (define (ide-setup)
    (set! LOG-FILE (open-file (list path: "IDE.txt" append: #t)))
    (log "IDE")
    (for-each setup-controller (iota IDE-CONTROLLERS))
    (log "Overall summary")
    (for-each
     (lambda (dev)
      (log "Device...")
      (log (ide-device-id dev))
      (log (ide-device-kind dev))
      (log (ide-device-model-num dev))
      )
      (list-devices)
     )
    ; (switch-over-driver)
    (cons IDE-INT handle-ide-int)
    (close-port LOG-FILE)
    (set! LOG-FILE 'NO-FILE))

  ; Create a list of ide devices
  (define (list-devices)
    (filter (flatten
              (vector->list (vector-map ide-controller-devices IDE-CTRL-VECT)))
            (o not device-absent?)))

  ))

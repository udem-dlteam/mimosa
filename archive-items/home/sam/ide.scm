;; The mimosa project
;; Ide controller base code
(define-library (ide)
(import (gambit) (utils) (low-level) (debug)) 
(export
  setup
  switch-over-driver
  handle-ide-int
  ide-write-sectors
  list-devices 
  ide-read-sectors)
(begin
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
  (define IDE-STATUS-BSY (<< 1 7))  ; Device busy bit
  (define IDE-STATUS-RDY (<< 1 6))  ; Device ready bit
  (define IDE-STATUS-DF (<< 1 5))   ; Device fault bit
  (define IDE-STATUS-DSC (<< 1 4))  ; Drive seek complete bit
  (define IDE-STATUS-DRQ (<< 1 3))  ; Data request bit
  (define IDE-STATUS-CORR (<< 1 2))  ; Corrected data bit
  (define IDE-STATUS-INDEX (<< 1 1))  ; Index bit
  (define IDE-STATUS-ERR 1)    ; Error bit
  (define IDE-ERROR-BBK   (<< 1 7)) ; Bad block mark detected in sector's ID field
  (define IDE-ERROR-UNC   (<< 1 6)) ; Uncorrectable data error encountered
  (define IDE-ERROR-IDNF  (<< 1 4)) ; Requested sector's ID field not found
  (define IDE-ERROR-ABRT  (<< 1 2)) ; Command aborted (status error or invalid cmd)
  (define IDE-ERROR-TK0NF (<< 1 1)) ; Track 0 not found during recalibrate command
  (define IDE-ERROR-AMNF  (<< 1 0)) ; Data address mark not found after ID field
  (define IDE-DEV-CTRL-SRST (<< 1 2)) ; Software reset bit
  (define IDE-DEV-CTRL-nIEN (<< 1 1)) ; Interrupt enable bit (0=enabled)
  (define IDE-DEV-HEAD-IBM #xa0)
  (define IDE-DEV-HEAD-LBA (fxior (<< 1 6) IDE-DEV-HEAD-IBM)) ; LBA address
  (define (IDE-DEV-HEAD-DEV x) (<< x 4)) ; Device index (0 or 1)
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
  (define IDE-CONTROLLER-PORTS (list
                                 IDE-CTRL-0
                                 IDE-CTRL-1
                                 IDE-CTRL-2
                                 IDE-CTRL-3))
  (define IDE-CONTROLLER-IRQS (list
                                IDE-IRQ-0
                                IDE-IRQ-1
                                IDE-IRQ-2
                                IDE-IRQ-3
                                ))
  (define (not-absent? status)
    (let ((mask (fxior IDE-STATUS-BSY IDE-STATUS-RDY  IDE-STATUS-DF 
                       IDE-STATUS-DSC  IDE-STATUS-DRQ)))
      (not (fx= (fxand status mask) mask)))) 
  (define-type ide-device
               id
               kind
               controller
               serial
               firmware-rev
               model-num
               ; ATA device information
               cylinders-per-disks
               heads-per-cylinder
               sectors-per-track
               total-sectors-chs
               total-sectors)
  (define-type ide-controller
               controller-id
               cpu-port
               irq
               devices 
               continuations-queue
               mut; condvar mutex
               cv ; condvar itself
               )
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
          (debug-write "Data address mark not found after ID field"))))

  ; Take a vector made of shorts and return a vector made
  ; of bytes (system endianness)
  (define (expand-wvect wvector)
    (let ((get-byte (lambda (i)
                      (let* ((even (fxeven? i))
                             (half-pos (if even 
                                           (fxhalve i)
                                           (fxhalve (- i 1))))
                             (val (vector-ref wvector half-pos)))
                        (b-chop (if (not even)
                                    (>> val 8)
                                    val))))))
      (build-vector (* 2 (vector-length wvector)) get-byte)))

  ; Take a vector of bytes and compresses it into a vector
  ; of shorts (system endianness)
  (define (compress-bvect b-vect)
    (let ((l (vector-length b-vect)))
      (build-vector (fxhalve l)
                    (lambda (idx)
                      (let ((b-idx (<< idx 1)))
                        (fxior (<< (vector-ref b-vect  (+ b-idx 1)) 8)
                               (vector-ref b-vect b-idx))))))) 

  ; Creates a read command for the int
  (define (ide-make-sector-read-command cpu-port target-vector cv mut)
    (lambda ()
      (let* ((data-reg (fx+ cpu-port IDE-DATA-REG))
             (stt-reg (fx+ cpu-port IDE-STATUS-REG))
             (alt-reg (fx+ cpu-port IDE-ALT-STATUS-REG))
             (status (inb stt-reg)))
        (if (mask status IDE-STATUS-ERR)
            (ide-handle-read-err cpu-port)
            (begin 
              (mutex-lock! mut)
              (for-each (lambda (i)
                          (vector-set! target-vector i (inw data-reg)))
                        (iota (vector-length target-vector)))
              (if (mask IDE-STATUS-DRQ (inb alt-reg))
                  (debug-write "Unknown error while reading..."))
              ; Signal we are ready
              (condition-variable-signal! cv)
              (mutex-unlock! mut))))))

  ; Flush the command cache of an ide device
  (define (ide-flush-cache device)
    (let* ((dev-id (ide-device-id device))
           (ctrl ((ide-device-controller device)))
           (cpu-port (ide-controller-cpu-port ctrl))
           (cmd-reg (fx+ cpu-port IDE-COMMAND-REG)))
      (disable-interrupts)
      (outb IDE-FLUSH-CACHE-CMD cmd-reg)
      (enable-interrupts)))


  ; Read `count` sectors from the ide device. 
  ; When the data is read, the continuation is called with
  ; a vector that corresponds to the data at `lba` (logical block addressing)
  ; The continuation might be called from another thread
  (define (ide-read-sectors device lba count)
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
               (cmd-reg (fx+ cpu-port IDE-COMMAND-REG))
               (stt-reg (fx+ cpu-port IDE-STATUS-REG))
               (q (ide-controller-continuations-queue ctrl))
               (count (min 256 count))
               (sz (<< count (- IDE-LOG2-SECTOR-SIZE 1)))
               (word-vector (make-vector sz 0)))
          (debug-write "IDE mutex L")
          (mutex-lock! mut)
          (debug-write "After IDE mutex L")
          (write (ide-make-sector-read-command cpu-port word-vector cv mut) q)
          (force-output q)
          (outb (b-chop (fxior IDE-DEV-HEAD-LBA 
                               (IDE-DEV-HEAD-DEV dev-id)
                               (>> lba 24))) head-reg)
          (outb (b-chop count) sect-count-reg)
          (outb (b-chop lba) sect-num-reg)
          (outb (b-chop (>> lba 8)) cyl-lo-reg)
          (outb (b-chop (>> lba 16)) cyl-hi-reg)
          (outb (b-chop IDE-READ-SECTORS-CMD) cmd-reg)
          ; Wait on condvar
          (debug-write "Wait on CV")
          (mutex-unlock! mut cv)
          ; this is the resulting vector
          (expand-wvect word-vector))

        (cont (make-vector 0 0))))

  (define (ide-init-devices)
    (make-list IDE-DEVICES-PER-CONTROLLER IDE-DEVICE-ABSENT))

  (define (device-absent? dev)
    (eq? dev IDE-DEVICE-ABSENT))

  (define (ide-write-sectors device lba buffer count)
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
           (q (ide-controller-continuations-queue ctrl))
           (write-lambda (lambda ()
                           (for-each (lambda (i)
                                       (outw (vector-ref compressed-vector i)  data-reg))
                                     (iota (<< 1 (- IDE-LOG2-SECTOR-SIZE 1))))))
           (count (min 256 count)))
      ; (push write-lambda q)
      (outb (b-chop (fxior IDE-DEV-HEAD-LBA 
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
      (write-lambda)
      ; Send a flush command
      (write (lambda () #t) q) ; nothing really to do, maybe add a signal later?
      (force-output q)
      (ide-flush-cache device)))

  ; Init the devices struct
  (define (ide-init-controllers)
    (list->vector (map (lambda (i)
                         (make-ide-controller
                           i ; the id
                           (list-ref IDE-CONTROLLER-PORTS i) ; the cpu-port
                           (list-ref IDE-CONTROLLER-IRQS i) ; the irq
                           (ide-init-devices)
                           (open-vector) ; make a 10 item queue
                           (make-mutex)
                           (make-condition-variable))) (iota IDE-CONTROLLERS))))

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

  (define IDE-CTRL-VECT (ide-init-controllers))

  (define (ide-delay cpu-port)
    (for-each (lambda (n)
                ; We read the alternative status reg.
                ; it doesnt erase it, and is the recommanded way of 
                ; waiting on an ide device
                (inb (fx+ cpu-port IDE-ALT-STATUS-REG)))
              (iota 4)))

  (define (swap-and-trim vect offset len)
    (let* ((idcs (iota len))
           (extract-char (lambda (idx)
                           (if (mask idx 1) 
                               (vector-ref vect (+ offset (>> idx 1)))
                               (>> 
                                 (vector-ref vect (+ offset (>> idx 1)))
                                 8))))
           (untrimmed (list->string (map (o integer->char extract-char) idcs))))
      (string-trim untrimmed)))

  (define (handle-ide-int controller-no)
    (let* ((ctrl (vector-ref IDE-CTRL-VECT controller-no))
           (q (ide-controller-continuations-queue ctrl))
           (cont (read q)))
      (if cont (cont) #f))) 

  (define (ide-make-device-setup-lambda controller devices)
    (lambda (dev-no)
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
                         (has-extended (mask (vector-ref id-vect 53) 1))
                         (cyl-per-dsk (if has-extended
                                          (vector-ref id-vect 54)
                                          (vector-ref id-vect 1)))
                         (heads-per-cyl (if has-extended
                                            (vector-ref id-vect 55)
                                            (vector-ref id-vect 3)))
                         (sect-per-trk (if has-extended 
                                           (vector-ref id-vect 56)
                                           (vector-ref id-vect 6)))
                         (total-sectors (fx+ 
                                          (<< (vector-ref id-vect 61) 16)
                                          (vector-ref id-vect 60)))
                         (total-sectors-chs (if has-extended
                                                (fx+ 
                                                  (<< (vector-ref id-vect 58) 16)
                                                  (vector-ref id-vect 57)) 0))
                         (device (make-ide-device
                                   dev-no
                                   (list-ref devices dev-no)
                                   (lambda () controller)
                                   serial-num
                                   firmware-rev
                                   model-num
                                   cyl-per-dsk
                                   heads-per-cyl
                                   sect-per-trk
                                   total-sectors-chs
                                   total-sectors)))
                    (ide-controller-set-device controller dev-no device))))))))

  ; Make a lambda to detect if a device is present on the ide
  ; controller whoses CPU port is the cpu-port in parameters
  (define (ide-make-device-detection-lambda controller controller-statuses cpu-port)
    (lambda (dev-no)
      (let* ((device-reg (fx+ cpu-port IDE-DEV-HEAD-REG)))
        (outb (fxior IDE-DEV-HEAD-IBM (IDE-DEV-HEAD-DEV dev-no)) device-reg)
        ; Send detection info
        (ide-delay cpu-port)
        (let* ((status (inb (fx+ cpu-port IDE-STATUS-REG))))
          (begin
            (vector-set! controller-statuses dev-no status)
            (if (not-absent? status)
                IDE-DEVICE-ATAPI
                IDE-DEVICE-ABSENT))))))

  ; Reset an ide device
  ; type: the type of the device
  ; ctrl-cpu-port: the cpu port the cpu port where to write out (of the ctrler)
  ; device-no: the device number
  ; returns true if the device was successfully reset, false otherwise
  (define (ide-reset-device type ctrl-cpu-port device-no)
    (let ((head-reg (fx+ ctrl-cpu-port IDE-DEV-HEAD-REG))
          (stt-reg (fx+ ctrl-cpu-port IDE-STATUS-REG)))
      (outb (fxior IDE-DEV-HEAD-IBM (IDE-DEV-HEAD-DEV device-no)) head-reg)
      (ide-delay ctrl-cpu-port)
      (and (fx= (fxand (inb stt-reg) IDE-STATUS-BSY) 0)
           (eq? type IDE-DEVICE-ATAPI))))
    
  (define (ide-reset-controller controller devices statuses candidates)
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
      (for-each (lambda (j)
                  (begin
                    (if (> candidates 0)
                        (begin
                          (for-each (lambda (device-no)
                                      (let ((dev-type (list-ref devices device-no)))
                                        ; add IDE device type
                                        (if (ide-reset-device dev-type cpu-port device-no)
                                            (begin
                                              (set! candidates (- candidates 1))
                                              (list-set! devices device-no IDE-DEVICE-ATA)))))
                                    (iota IDE-DEVICES-PER-CONTROLLER))
                          (thread-sleep! (until-has-elapsed 1 TIME-UNIT-MS))))))
                (iota 30000))
      (if (> (apply + (map (lambda (dev) (if (eq? dev IDE-DEVICE-ATA) 1 0))
                           devices))
             0)
          ; There are candidates
          (begin
            (for-each (lambda (dev)
                        (begin
                          (outb (fxior IDE-DEV-HEAD-IBM (IDE-DEV-HEAD-DEV dev)) head-reg)
                          (ide-delay cpu-port)
                          (if (and (fx= (inb cyl-lo-reg) #x14)
                                   (fx= (inb cyl-hi-reg) #xeb)
                                   (eq? (list-ref devices dev)
                                        IDE-DEVICE-ATA))
                              ; Update the list: this is an ATAPI device
                              (list-set! devices dev IDE-DEVICE-ATAPI)))) 
                      (iota IDE-DEVICES-PER-CONTROLLER))
            (for-each (lambda (dev)
                        (let ((dev-type (list-ref devices dev)))
                          (if (eq? IDE-DEVICE-ATA dev-type)
                              (if (fx= 0 (vector-ref statuses dev))
                                  ; A zero status ATA is absent
                                  (list-set! devices dev IDE-DEVICE-ABSENT)
                                  ; Make sure the device is present
                                  (begin
                                    (outb (fxior IDE-DEV-HEAD-IBM (IDE-DEV-HEAD-DEV dev)) head-reg) 
                                    (ide-delay cpu-port)
                                    ; TODO: remove these magic numbers
                                    (outb #x58 err-reg)
                                    (outb #xA5 cyl-lo-reg)
                                    (if (or (fx= #x58 (inb err-reg))
                                            (not (fx= #xA5 (inb cyl-lo-reg))))
                                        (list-set! devices dev IDE-DEVICE-ABSENT)))))))
                      (iota IDE-DEVICES-PER-CONTROLLER))))
      ; Setup command q
      (let ((setup-device (ide-make-device-setup-lambda controller devices)))
        (for-each setup-device (iota IDE-DEVICES-PER-CONTROLLER)))
      ; (for-each (lambda (dev-no)
      ;             (ide-controller-if-device? controller dev-no display) 
      ;            ) (iota IDE-DEVICES-PER-CONTROLLER)))
      ; Enable ints on the controller
      (outb 0 ctrl-reg)))

  ; Setup an ide controller
  (define (setup-controller no)
    (begin
      (let* ((controller (vector-ref IDE-CTRL-VECT no))
             (cpu-port (ide-controller-cpu-port controller))
             (irq (ide-controller-irq controller)))
        (let* ((statuses (make-vector IDE-DEVICES-PER-CONTROLLER 0))
               (ide-detect-device (ide-make-device-detection-lambda no statuses cpu-port))
               (devices (map ide-detect-device (iota IDE-DEVICES-PER-CONTROLLER)))
               (candidates (apply + (map (lambda (device)
                                           (if (eq? device IDE-DEVICE-ABSENT)
                                               0
                                               1))
                                         devices))))
          (if (> candidates 0)
              (ide-reset-controller controller devices statuses candidates)
              #f)))))

  ; Switch the IDE management from the C kernel to the scheme kernel
  (define (switch-over-driver)
    (open-input-file "/cut"))

  (define (setup)
    (begin
      (for-each setup-controller (iota IDE-CONTROLLERS))
      ; TODO: line 865+ of the cpp
      #t))

 ; Create a list of ide devices
 (define (list-devices)
  (filter (flatten (vector->list (vector-map ide-controller-devices IDE-CTRL-VECT)))
          (o not device-absent?)))

))

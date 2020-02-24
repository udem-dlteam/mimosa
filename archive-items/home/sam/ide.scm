;; The mimosa project
;; Ide controller base code

(define IDE-DEVICE-ABSENT 0)
(define IDE-DEVICE-ATA    1)
(define IDE-DEVICE-ATAPI  2)

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

(define IDE-STATUS-BSY (fxarithmetic-shift 1 7))  ; Device busy bit
(define IDE-STATUS-RDY (fxarithmetic-shift 1 6))  ; Device ready bit
(define IDE-STATUS-DF (fxarithmetic-shift 1 5))   ; Device fault bit
(define IDE-STATUS-DSC (fxarithmetic-shift 1 4))  ; Drive seek complete bit
(define IDE-STATUS-DRQ (fxarithmetic-shift 1 3))  ; Data request bit
(define IDE-STATUS-CORR (fxarithmetic-shift 1 2))  ; Corrected data bit
(define IDE-STATUS-INDEX (fxarithmetic-shift 1 1))  ; Index bit
(define IDE-STATUS-ERR (fxarithmetic-shift 1 0))    ; Error bit

(define IDE-ERROR-BBK   (fxarithmetic-shift 1 7)) ; Bad block mark detected in sector's ID field

(define IDE-ERROR-UNC   (fxarithmetic-shift
                          1 6)) ; Uncorrectable data error encountered

(define IDE-ERROR-IDNF  (fxarithmetic-shift
                          1 4)) ; Requested sector's ID field not found

(define IDE-ERROR-ABRT  (fxarithmetic-shift
                          1 2)) ; Command aborted (status error or invalid cmd)

(define IDE-ERROR-TK0NF (fxarithmetic-shift
                          1 1)) ; Track 0 not found during recalibrate command

(define IDE-ERROR-AMNF  (fxarithmetic-shift
                          1 0)) ; Data address mark not found after ID field

(define IDE-DEV-CTRL-SRST (fxarithmetic-shift
                            1 2)) ; Software reset bit

(define IDE-DEV-CTRL-nIEN (fxarithmetic-shift
                            1 1)) ; Interrupt enable bit (0=enabled)

(define IDE-DEV-HEAD-IBM #xa0)
(define IDE-DEV-HEAD-LBA (fxior (fxarithmetic-shift 1 6) IDE-DEV-HEAD-IBM)) ; LBA address
(define (IDE-DEV-HEAD-DEV x) (fxarithmetic-shift x 4)) ; Device index (0 or 1)

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

(define IDE-CTRL-VECT (vector (vector IDE-CTRL-0 IDE-IRQ-0)
                              (vector IDE-CTRL-1 IDE-IRQ-1)
                              (vector IDE-CTRL-2 IDE-IRQ-2)
                              (vector IDE-CTRL-3 IDE-IRQ-3)))

                            

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
             devices 
             commands ; probably not necessary anymore
             commands-convdar)

(define (handle-ide-int ide-id)
 (debug-write (string-append "IDE int no " (number->string ide-id))))

(define (ide-setup-controller no)
 (debug-write (string-append "Setup controller " (number->string no))))

(define (ide-setup)
 (begin
   (for-each ide-setup-controller (iota IDE-CONTROLLERS))
   ; TODO: line 865+ of the cpp
   #t))

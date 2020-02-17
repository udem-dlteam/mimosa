;; The mimosa project
;; UART driver
(define DEFAULT-BAUD-RATE 115200)

; /* COM1 */
(define COM1-PORT-BASE #x3f8)
(define COM1-IRQ 4)
; /* COM2 */
(define COM2-PORT-BASE #x2f8)
(define COM2-IRQ 3)
; /* COM3 */
(define COM3-PORT-BASE #x3e8 )
(define COM3-IRQ 4)
 ; /* COM4 */
(define COM4-PORT-BASE #x2e8)
(define COM4-IRQ 3)

(define (COM-PORT->CPU-PORT n)
 (cond ((= n 1)
        COM1-PORT-BASE)
       ((= n 2)
        COM2-PORT-BASE)
       ((= n 3)
        COM3-PORT-BASE)
       (else
        COM4-PORT-BASE)))

(define (CPU-PORT->COM-PORT cpu-port)
  (cond ((= cpu-port COM1-PORT-BASE)
         1)
        ((= cpu-port COM2-PORT-BASE)
         2)
        ((= cpu-port COM3-PORT-BASE)
         3)
        ((= cpu-port COM4-PORT-BASE)
         4)))

(define (COM-PORT->IRQ-NO port)
  (if (= (modulo port 2) 1)
      4
      3))

(define UART-8250-RHR 0)
(define UART-8250-THR 0)
(define UART-8250-IER 1)
(define UART-8250-IIR 2)
(define UART-8250-FCR 2)
(define UART-8250-LCR 3)
(define UART-8250-MCR 4)
(define UART-8250-LSR 5)
(define UART-8250-MSR 6)
(define UART-8250-SCR 7)

; to set the DLAB you have to set last LCR byte to 0 or 1

(define UART-8250-DLL 0) ;; divisor latch lo byte (needs DLAB at 0)
(define UART-8250-DLH 1) ;; divisor latch hi byte (needs DLAB at 1)

(define (DIV-DLH baud) (fxarithmetic-shift-right (/ 115200 baud) 8))
(define (DIV-DLL baud) (fxand (/ 115200 baud) #xFF))

(define UART-8250-LSR-ERF (fxarithmetic-shift 1 7))
(define UART-8250-LSR-TEMT (fxarithmetic-shift 1 6))
(define UART-8250-LSR-THRE (fxarithmetic-shift 1 5))
(define UART-8250-LSR-BI (fxarithmetic-shift 1 4))
(define UART-8250-LSR-FE (fxarithmetic-shift 1 3))
(define UART-8250-LSR-PE (fxarithmetic-shift 1 2))
(define UART-8250-LSR-OE (fxarithmetic-shift 1 1))
(define UART-8250-LSR-DR (fxarithmetic-shift 1 0))

; Line Status Register (LSR) interrupt cause 
(define (UART-LSR-DATA-AVAILABLE x) (fxand x UART-8250-LSR-DR))
(define (UART-LSR-OVERRUN-ERROR x) (fxand x UART-8250-LSR-OE))
(define (UART-LSR-PARITY-ERROR x) (fxand x UART-8250-LSR-PE))
(define (UART-LSR-FRAMING-ERROR x) (fxand x UART-8250-LSR-FE))
(define (UART-LSR-BREAK-INTERRUPT x) (fxand x UART-8250-LSR-BI))
(define (UART-LSR-CAN-RECEIVE x) (fxand x UART-8250-LSR-THRE))
(define (UART-LSR-ALL-CAR-TRANSMITTED x) (fxand x UART-8250-LSR-TEMT))
(define (UART-LSR-ERROR-IN-RECEIVED-FIFO x) (fxand x UART-8250-LSR-ERF))

; Modem Status Register (MSR) bit flags
(define UART-8250-MSR-CD (fxarithmetic-shift 1 7))
(define UART-8250-MSR-RI (fxarithmetic-shift 1 6))
(define UART-8250-MSR-DSR (fxarithmetic-shift 1 5))
(define UART-8250-MSR-CTS (fxarithmetic-shift 1 4))
(define UART-8250-MSR-DDCD (fxarithmetic-shift 1 3))
(define UART-8250-MSR-TERI (fxarithmetic-shift 1 2))
(define UART-8250-MSR-DDSR (fxarithmetic-shift 1 1))
(define UART-8250-MSR-DCTS (fxarithmetic-shift 1 0))

; Modem Status Register (MSR) interrupt causes
(define (UART-MSR-CARRIER-DETECT x) (fxand x UART-8250-MSR-CD))
(define (UART-MSR-RING-INDICATOR x) (fxand x UART-8250-MSR-RI))
(define (UART-MSR-DATA-SET-READY x) (fxand x UART-8250-MSR-DSR))
(define (UART-MSR-CLEAR-TO-SEND x) (fxand x UART-8250-MSR-CTS))
(define (UART-MSR-DELTA-DATA-CARRIER-DETECT x) (fxand x UART-8250-MSR-DDCD))
(define (UART-MSR-TRAILING-EDGE-RING-INDICATOR x) (fxand x UART-8250-MSR-TERI))
(define (UART-MSR-DELTA-DATA-SET-READY x) (fxand x UART-8250-MSR-DDSR))
(define (UART-MSR-DELTA-CLEAR-TO-SEND x) (fxand x UART-8250-MSR-DCTS))

(define (UART-IIR-PENDING x) (fxnot (fxand x (fxarithmetic-shift 1 0))))
(define (UART-IIR-IS-64-BIT-FIFO x) (fxand x (fxarithmetic-shift 1 5)))
(define (UART-IIR-GET-CAUSE x) (fxarithmetic-shift-right (fxand x #xE) 1))
(define (UART-IIR-GET-FIFO-STATE x) (fxarithmetic-shift-right (fxand x #xC0) 6))

(define (UART-THR-GET-ACTION x) (fxand x UART-8250-LSR-THRE))

; Interrupt Identification Register (IIR) interrupt cause
(define UART-IIR-MODEM 0)
(define UART-IIR-TRANSMITTER-HOLDING-REG 1)
(define UART-IIR-DATA-AVAIL 2)
(define UART-IIR-RCV-LINE 3)
(define UART-IIR-TIMEOUT 6)
; Interrupt Identification Register (IIR) FIFO status
(define UART-IIR-FIFO-NO-FIFO 0)
(define UART-IIR-FIFO-RESERVED 1)
(define UART-IIR-FIFO-ENABLED-ERROR 2)
(define UART-IIR-FIFO-ENABLED 3)

(define COM-PORT-STATUS-EXISTS (fxarithmetic-shift 1 0))
(define COM-PORT-STATUS-OPEN (fxarithmetic-shift 1 1))
(define COM-PORT-STATUS-FULL (fxarithmetic-shift 1 2))
(define COM-PORT-STATUS-WAITING (fxarithmetic-shift 1 3))
(define COM-PORT-STATUS-FORCIBLY-CLOSED (fxarithmetic-shift 1 4))
(define COM-PORT-STATUS-READ-READY (fxarithmetic-shift 1 5))
(define COM-PORT-STATUS-WRITE-READY (fxarithmetic-shift 1 6))
(define COM-PORT-STATUS-RESERVED1 (fxarithmetic-shift 1 7))


(define (##make-port n)
  ; Port datastructure arrangement:
  ; 0 : the COM port number
  ; 1 : if the port is opened 
  ; 2 : the write queue (goes out)
  ; 3 : the read queue (comes in)
  (vector n #f (make 4) (make 4)))

(define port-data (vector (##make-port 1)
                          (##make-port 2)
                          (##make-port 3)
                          (##make-port 4)))

; Get the port data structure for the specified com port
(define (get-port-data com-port)
  (if (and (<= com-port 4) (>= com-port 1))
      (vector-ref port-data (- com-port 1))
      #f))

; ---------------------------------------------
;                 IN PORT 
; ---------------------------------------------

; Check if the uart port has been opened
(define (uart-opened? port-data)
 (vector-ref port-data 1))

; Pop a character from the in buffer or #f if empty
(define (uart-port-next-char port-data)
 (pop (vector-ref port-data 2)))

; ---------------------------------------------
;                 OUT PORT 
; ---------------------------------------------

; Push a character in the in buffer
(define (uart-port-push-char port-data char)
  (push char (vector-ref port-data 3)))

(define (uart-port-next-char-out port-data)
 (pop (vector-ref port-data 3)))

; ---------------------------------------------

(define (open-port! port-data)
 (vector-set! port-data 1 #t))


; Return the model number of the uart port
(define (identify-material port)
  (let* ((cpu-port (COM-PORT->CPU-PORT port))
         (fcr-reg (+ cpu-port UART-8250-FCR))
         (iir-reg (+ cpu-port UART-8250-IIR))
         (scr-reg (+ cpu-port UART-8250-SCR)))
    (outb #xE7 fcr-reg)
    (let* ((iir-test (inb iir-reg)))
      (cond ((fx> (fxand iir-test #x20) 0)
             16750)
            ((fx> (fxand iir-test #x80))
             16550)
            ((fx> (fxand iir-test #x40))
             16550)
            (else
              (outb #x2A scr-reg)
              (if (fx= (inb scr-reg) #x2A)
                  16450
                  8250))))))

(define (uart-enable-dlab register)
  (let ((current-val (inb register)))
    (begin
      (outb (fxior #x80 current-val))
      #t)))

(define (uart-disable-dlab register)
  (let ((lcr-val (fxand (inb register) #x7F)))
    (outb lcr-val register)))

(define (uart-set-baud port baud)
  (let* ((cpu-port (COM-PORT->CPU-PORT port))
         (lcr-reg (+ cpu-port UART-8250-LCR))
         (divisor-latch-low-reg (+ cpu-port UART-8250-DLL))
         (divisor-latch-high-reg (+ cpu-port UART-8250-DLH)))
    (uart-enable-dlab lcr-reg)
    (outb (DIV-DLL baud) divisor-latch-low-reg)
    (outb (DIV-DLH baud) divisor-latch-high-reg)
    (uart-disable-dlab lcr-reg)
    #t))

(define (uart-get-baud port)
 (let* ((cpu-port (COM-PORT->CPU-PORT))
        (lcr-reg (+ cpu-port UART-8250-LCR))
        (divisor-latch-low-reg (+ cpu-port UART-8250-DLL))
        (divisor-latch-high-reg (+ cpu-port UART-8250-DLH))
        (div-lo (inb divisor-latch-lo-reg))
        (div-hi (inb divisor-latch-high-reg)))
  (if (= 0 (+ div-lo div-hi))
   0
   (begin
     (uart-enable-dlab lcr-reg)
     (let* ((div-hi (fxarithmetic-shift div-hi 8))
            (baud (/ 115200 (+ div-hi div-lo))))
       (uart-disable-dlab lcr-reg)
       baud)))))


(define (uart-read-rhr cpu-port)
 (let* ((data (inb (+ cpu-port UART-8250-RHR))))
  (write-char-stdin (integer->char data))))

(define (uart-read-msr cpu-port)
 (let ((msr-reg (+ cpu-port UART-8250-MSR)))
  (inb msr-reg)))

(define (handle-thr cpu-port)
 (if (UART-THR-GET-ACTION (inb (+ cpu-port UART-8250-LSR)))
  ; We can write to THR
  (let* ((com-port (CPU-PORT->COM-PORT cpu-port))
         (port-data (get-port-data com-port))
         (next-char (uart-port-next-char-out port-data))
         (thr-reg (+ cpu-port UART-8250-THR)))
   (if next-char
    (outb next-char thr-reg)
    #f))
  ; read from IIR
  #f))

(define (uart-handle-cause cpu-port cause)
  (cond ((= cause UART-IIR-MODEM)
         ; Modem Status
         ; Caused by : Change in clear to send, data set
         ;             ready, ring indicator, or received
         ;             line signal detect signals.
         ; priority :lowest
         ; Reading Modem Status Register (MSR)
         (read-msr cpu-port))
        ((= cause UART-IIR-TRANSMITTER-HOLDING-REG)
         ; Transmitter empty
         ; Caused by : The transmitter finishes sending
         ;             data and is ready to accept additional data.
         ; priority : next to lowest
         ; Reading interrupt indentification register(IIR)
         ; or writing to Transmit Holding Buffer (THR)
         (begin
           (display "THR")
           (handle-thr cpu-port))
         ((= cause UART-IIR-RCV-LINE)
          ; Error or Break
          ; caused by : Overrun error, parity error, framing
          ;             error, or break interrupt.
          ; priority : highest
          ; reading line status register
          (display "RCV LINE"))
         ((= cause UART-IIR-DATA-AVAIL)
          ; Data Available
          ; caused by : Data arriving from an external
          ;             source in the Receive Register.
          ; priority : next to highest
          ; timeout is available on new model.
          ; This means that we need to read data
          ; before the connection timeouts
          ; reading receive Buffer Register(RHR)
          (uart-read-rhr cpu-port))
         ((= cause UART-IIR-TIMEOUT)
          (display "Timeout"))
         (else
           (display "Unknown IIR status"))))

(define (handle-uart-int port)
 (let* ((cpu-port (COM-PORT->CPU-PORT port))
        (iir (inb (+ cpu-port UART-8250-IIR)))
        (cause (UART-IIR-GET-CAUSE iir)))
  (uart-handle-cause cpu-port cause)))


(define (uart-do-init port)
  (let* ((cpu-port (COM-PORT->CPU-PORT port))
         (ier-reg (+ cpu-port UART-8250-IER))
         (iir-reg (+ cpu-port UART-8250-IIR))
         (mcr-reg (+ cpu-port UART-8250-MCR))
         (lcr-reg (+ cpu-port UART-8250-LCR))
         (port-data (get-port-data port)))
    (outb #x00 ier-reg)
    (outb #x03 lcr-reg)
    (uart-set-baud port DEFAULT-BAUD-RATE)
    (outb #x0F ier-reg)
    (outb #x8E iir-reg)
    (outb #x08 mcr-reg)
    (enable-irq (COM-PORT->IRQ-NO port))
    (open-port! port-data)))
    

(define (uart-init-port port)
 (if (and (fx>= port 1)
          (fx<= port 4))
    (uart-do-init port )
    #f))

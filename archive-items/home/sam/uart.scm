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

(define UART-8250-RHR 0)
(define UART-8250-THR 0)
(define UART-8250-IER 1)
(define UART-8250-IIR 2)
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
(define UART-8250-LSR-TEMT (fxarithmetic-shift 6))
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

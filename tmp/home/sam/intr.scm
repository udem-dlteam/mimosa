; Mimosa
; Université de Montréal
; Marc Feeley, Samuel Yvon
(define-library (intr)
(import
  (gambit)
  (low-level))
(export
  enable-irq
  disable-irq
  ack-irq)
(begin
  (define PIC-PORT-MASTER-ICW1 #x20)
  (define PIC-PORT-SLAVE-ICW1 #xa0)
  (define PIC-PORT-MASTER-ICW2 #x21)
  (define PIC-PORT-SLAVE-ICW2 #xa1)
  (define PIC-PORT-MASTER-ICW3 #x21)
  (define PIC-PORT-SLAVE-ICW3 #xa1)
  (define PIC-PORT-MASTER-ICW4 #x21)
  (define PIC-PORT-SLAVE-ICW4 #xa1)
  (define (PIC-ICW1 params) (fxor (fxarithmetic-shift 1 4) params))
  (define PIC-ICW1-LTIM (fxarithmetic-shift 1 3)) ; Level triggered mode
  (define PIC-ICW1-SNGL (fxarithmetic-shift
                          1 1)) ; Single PIC in system
  (define PIC-ICW1-ICW4 (fxarithmetic-shift
                          1 0)) ; ICW4 necessary
  (define (PIC-ICW2 offset) (fxarithmetic-shift offset 3))
  (define (PIC-MASTER-ICW3 slaves) slaves)
  (define (PIC-MASTER-ICW3-SLAVE id) (fxarithmetic-shift 1 id))
  (define (PIC-SLAVE-ICW3 id) id)
  (define (PIC-ICW4 params) (fxor params (fxarithmetic-shift 1 4)))
  (define PIC-ICW4-SFNM (fxarithmetic-shift 1 4))   ; Operation in Special Fully Nested Mode
  (define PIC-ICW4-BUF (fxarithmetic-shift 1 3))    ; Operation in Buffered Mode
  (define PIC-ICW4-MASTER (fxarithmetic-shift 1 2)) ; Master PIC
  (define PIC-ICW4-AEOI (fxarithmetic-shift 1 1))   ; Automatic EOI
  (define PIC-ICW4-8086 (fxarithmetic-shift 1 0))   ; Operation in 8086 mode
  ; For Operation Command Words (OCW)
  (define PIC-PORT-MASTER-OCW1 #x21)
  (define PIC-PORT-SLAVE-OCW1 #xa1)
  (define PIC-PORT-MASTER-OCW2 #x20)
  (define PIC-PORT-SLAVE-OCW2 #xa0)
  (define PIC-PORT-MASTER-OCW3 #x20)
  (define PIC-PORT-SLAVE-OCW3 #xa0)
  (define (PIC-OCW1-MASK n) (fxarithmetic-shift 1 n))
  (define PIC-OCW2-NONSPECIFIC-EOI (fxarithmetic-shift 1 5))
  (define (PIC-OCW2-SPECIFIC-EOI level) (+ 96 level))
  ; For Interrupt Mask Register
  (define PIC-PORT-MASTER-IMR #x21)
  (define PIC-PORT-SLAVE-IMR #xa1)
  ; Interrupt request numbers
  (define PIC-MASTER-IRQ0 0) ; Programmable Interval Timer (counter 0)
  (define PIC-MASTER-IRQ1 1) ; Keyboard
  (define PIC-MASTER-IRQ2 2) ; Slave Programmable Interrupt Controller
  (define PIC-MASTER-IRQ3 3) ; Serial Port (COM2)
  (define PIC-MASTER-IRQ4 4) ; Serial Port (COM1)
  (define PIC-MASTER-IRQ5 5) ; ?
  (define PIC-MASTER-IRQ6 6) ; Floppy Disk Controller
  (define PIC-MASTER-IRQ7 7) ; Parallel Port (or a master "lost interrupt")
  (define PIC-SLAVE-IRQ8  0) ; Real Time Clock
  (define PIC-SLAVE-IRQ9  1) ; ?
  (define PIC-SLAVE-IRQ10 2) ; ?
  (define PIC-SLAVE-IRQ11 3) ; ?
  (define PIC-SLAVE-IRQ12 4) ; PS/2 Mouse
  (define PIC-SLAVE-IRQ13 5) ; Math Coprocessor
  (define PIC-SLAVE-IRQ14 6) ; Hard Disk Drive
  (define PIC-SLAVE-IRQ15 7) ; ? (or a slave "lost interrupt")

  (define CPU-EX-DIV-BY-ZERO #x00)
  (define CPU-EX-DEBUG #x01)
  (define CPU-EX-NMI #x02)
  (define CPU-EX-BREAKPOINT #x03)
  (define CPU-EX-OVERFLOW #x04)
  (define CPU-EX-BOUND-RANGE-EXCEEDED #x05)
  (define CPU-EX-INVALID-OPCODE #x06)
  (define CPU-EX-DEV-NOT-AVAIL #x07)
  (define CPU-EX-DOUBLE-FAULT #x08)
  (define CPU-EX-COPROC-SEG-OVERRUN #x09)
  (define CPU-EX-INVALID-TSS #x0A)
  (define CPU-EX-SEGMENT-NO-PRESENT #x0B)
  (define CPU-EX-STACK-SEGMENT-FAULT #x0C)
  (define CPU-EX-GENERAL-PROTECTION-FAULT #x0D)
  (define CPU-EX-PAGE-FAULT #x0E)
  (define CPU-EX-RESERVED #x0F)

  (define (enable-irq n)
    (let ((j (if (fx< n 8)
                 n
                 (- n 8))))
      (outb (fxand (inb PIC-PORT-MASTER-IMR) (fxnot (PIC-OCW1-MASK n))) PIC-PORT-MASTER-OCW1)))

  (define (disable-irq n)
    (let ((j (if (fx< n 8)
                 n
                 (- n 8))))
      (outb (inb (fxor PIC-PORT-MASTER-IMR (PIC-OCW1-MASK n))) PIC-PORT-MASTER-OCW1)))

  (define (ack-irq n)
    (if (fx< n 8)
        (outb (PIC-OCW2-SPECIFIC-EOI n) PIC-PORT-MASTER-OCW2)
        (begin
          (outb (PIC-OCW2-SPECIFIC-EOI (- n 8)) PIC-PORT-SLAVE-OCW2)
          (outb (PIC-OCW2-SPECIFIC-EOI PIC-MASTER-IRQ2) PIC-PORT-MASTER-OCW2))))
  ))

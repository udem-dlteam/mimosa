
(define-library (low-level)
                (import (gambit)) 
                (export import-test
                        inb
                        outb
                        inw
                        outw
                        enable-interrupts
                        disable-interrupts)
    (begin
      (include "/dsk1/gambit/lib/_asm#.scm")
      (include "/dsk1/gambit/lib/_x86#.scm")
      (include "/dsk1/gambit/lib/_codegen#.scm")
      ;;;----------------------------------------------------

      ;; Convert a u8vector containing machine code into a
      ;; Scheme procedure taking 0 to 3 arguments.  Calling
      ;; the Scheme procedure will execute the machine code
      ;; using the C calling convention.

      (define (u8vector->procedure code)
        (let ((mcb (##make-machine-code-block code)))
          (lambda (#!optional (arg1 0) (arg2 0) (arg3 0))
            (##machine-code-block-exec mcb arg1 arg2 arg3))))

      ;; Create a new code generation context.  The format of
      ;; the resulting assembly code listing can also be
      ;; specified, either 'nasm, 'gnu, or #f (no listing,
      ;; which is the default).

      (define (make-cgc #!optional (format #f))
        (let ((cgc (make-codegen-context)))
          (asm-init-code-block cgc 0 endianness)
          (codegen-context-listing-format-set! cgc format)
          (x86-arch-set! cgc arch)
          cgc))

      (define arch 'x86-32)
      (define endianness 'le)

      (define (asm gen #!optional (format #f))
        (let ((cgc (make-cgc format)))
          (gen cgc)
          (let ((code (asm-assemble-to-u8vector cgc)))
            (if format
                (asm-display-listing cgc (current-error-port) #t))
            (u8vector->procedure code))))

      (define (import-test)
        (display "IMPORT-TEST"))

      ;; Implement interface to IN and OUT x86 instructions
      (define inb ;; parameter: port number
        (asm
          (lambda (cgc)
            (x86-mov   cgc (x86-edx) (x86-mem 4 (x86-esp))) ; Fetch an int32 in mem
            (x86-sar   cgc (x86-edx) (x86-imm-int 2))       ; 
            (x86-mov   cgc (x86-eax) (x86-imm-int 0))
            (x86-in-dx cgc (x86-al))
            (x86-shl   cgc (x86-eax) (x86-imm-int 2))
            (x86-ret   cgc))))

      (define inw
        (asm
          (lambda (cgc)
            (x86-mov   cgc (x86-edx) (x86-mem 4 (x86-esp))) ; Fetch an int32 in mem
            (x86-sar   cgc (x86-edx) (x86-imm-int 2))       ; 
            (x86-mov   cgc (x86-eax) (x86-imm-int 0))
            (x86-in-dx cgc (x86-ax)) ; inw takes ax in arg
            (x86-shl   cgc (x86-eax) (x86-imm-int 2))
            (x86-ret   cgc))))

      (define outb ;; parameters: value and port number
        (asm
          (lambda (cgc)
            (x86-mov   cgc (x86-edx) (x86-mem 8 (x86-esp)))
            (x86-sar   cgc (x86-edx) (x86-imm-int 2))
            (x86-mov   cgc (x86-eax) (x86-mem 4 (x86-esp)))
            (x86-sar   cgc (x86-eax) (x86-imm-int 2))
            (x86-out-dx cgc (x86-al))
            (x86-shl   cgc (x86-eax) (x86-imm-int 2))
            (x86-ret   cgc))))

      (define outw ;; parameters: value and port number
        (asm
          (lambda (cgc)
            (x86-mov   cgc (x86-edx) (x86-mem 8 (x86-esp))) ; load the port number
            (x86-sar   cgc (x86-edx) (x86-imm-int 2))
            (x86-mov   cgc (x86-eax) (x86-mem 4 (x86-esp))) ; load the value
            (x86-sar   cgc (x86-eax) (x86-imm-int 2)) ; unpack
            (x86-out-dx cgc (x86-ax))
            (x86-shl   cgc (x86-eax) (x86-imm-int 2))
            (x86-ret   cgc))))

      (define enable-interrupts
        (asm (lambda (cgc) (x86-sti cgc) (x86-ret cgc))))

      (define disable-interrupts
        (asm (lambda (cgc)  (x86-cli cgc) (x86-ret cgc))))
      ))

;;;----------------------------------------------------------------------------

(define (startup-repl-on-port port) ;; compatible with old _repl.scm
  (let ((t
         (make-thread
          (lambda ()
            (let ((repl-channel (##make-repl-channel-ports port port #;port)))
               (##vector-set! (current-thread) 28 repl-channel))
            (##repl-debug-main)))))
    (thread-start! t)))

(define (startup-repl-on-port2 port) ;; compatible with new _repl.scm
  (let ((t
         (make-thread
          (lambda ()
            (let ((repl-channel (##make-repl-channel-ports port port port)))
               (##vector-set! (current-thread) 28 repl-channel))
            (##repl-debug-main)))))
    (thread-start! t)))

;;;----------------------------------------------------------------------------

;; Convert a u8vector containing machine code into a Scheme procedure
;; taking 0 to 3 arguments.  Calling the Scheme procedure will execute
;; the machine code using the C calling convention.

(define (u8vector->procedure
         code
         #!optional
         (fixup-locs '#u8(0))
         (fixup-objs '#()))
  (machine-code-block->procedure
   (u8vector->machine-code-block code fixup-locs fixup-objs)))

(define (machine-code-block->procedure mcb)
  (lambda (#!optional (arg1 0) (arg2 0) (arg3 0))
    (##machine-code-block-exec mcb arg1 arg2 arg3)))

;; Convert a u8vector containing machine code into an executable
;; machine code block.

(define (u8vector->machine-code-block
         code
         #!optional
         (fixup-locs '#u8(0))
         (fixup-objs '#()))
  (let ((mcb (##make-machine-code-block code)))
    (##machine-code-fixup mcb fixup-locs fixup-objs)
    mcb))

;; Execute the machine code passing it 0 to 3 arguments (using the C
;; parameter passing convention).

(define (exec-machine-code
         code
         #!optional
         (fixup-locs '#u8(0))
         (fixup-objs '#())
         (arg1 0)
         (arg2 0)
         (arg3 0))
  ((u8vector->procedure code fixup-locs fixup-objs) arg1 arg2 arg3))

;;;----------------------------------------------------------------------------

;; Return the architecture of the processor by executing some binary
;; machine code that behaves differently on x86-32, x86-64 and ARM.
;;
;;   x86-32 for Intel x86-32
;;   x86-64 for Intel x86-64
;;   arm    for ARM

(define (auto-detect-arch)

  (define auto-detect-arch-code '#u8(
                    ;;       ARM              X86-32            X86-64
                    ;;
#xEB #x0A #xA0 #xE3 ;;      mov r0,#962560    jmp x86           jmp x86
#x03 #x00 #xE0 #xE3 ;;      mov r0,#-4        ...               ...
#x1E #xFF #x2F #xE1 ;;      bx  lr            ...               ...
                    ;; x86:
#x48                ;;                        dec eax           xor rax,rax
#x31 #xC0           ;;                        xor eax,eax
#x48                ;;                        dec eax           inc rax
#xFF #xC0           ;;                        inc eax
#xC1 #xE0 #x02      ;;                        shl eax,0x2       shl eax,0x2
#xC3                ;;                        ret               ret
                    ;;
                    ;;      returns -4        returns 0         returns 4
))

  (case (exec-machine-code auto-detect-arch-code)
    ((-1) 'arm)
    ((0)  'x86-32)
    ((1)  'x86-64)
    (else #f)))

;;;----------------------------------------------------------------------------

(define arch (auto-detect-arch))
(define endianness 'le)

;;;----------------------------------------------------------------------------

;; Import compiler's codegen, asm and x86 modules.

(namespace ("_codegen#"

make-codegen-context
codegen-context-listing-format-set!
codegen-context-fixup-locs->vector
codegen-context-fixup-objs->vector

))

(namespace ("_asm#"

asm-init-code-block
asm-assemble-to-u8vector
asm-display-listing

))

(namespace ("_x86#"

x86-implement

x86-register-name
x86-reg?
x86-reg8?
x86-reg8-h?
x86-xmm?
x86-mm?
x86-fpu?
x86-reg16?
x86-reg32?
x86-reg64?
x86-reg-field
x86-reg8
x86-reg16
x86-reg32
x86-reg64
x86-fpu
x86-reg-width

x86-al
x86-cl
x86-dl
x86-bl
x86-ah
x86-ch
x86-dh
x86-bh
x86-spl
x86-bpl
x86-sil
x86-dil
x86-r8b
x86-r9b
x86-r10b
x86-r11b
x86-r12b
x86-r13b
x86-r14b
x86-r15b

x86-ax
x86-cx
x86-dx
x86-bx
x86-sp
x86-bp
x86-si
x86-di
x86-r8w
x86-r9w
x86-r10w
x86-r11w
x86-r12w
x86-r13w
x86-r14w
x86-r15w

x86-eax
x86-ecx
x86-edx
x86-ebx
x86-esp
x86-ebp
x86-esi
x86-edi
x86-r8d
x86-r9d
x86-r10d
x86-r11d
x86-r12d
x86-r13d
x86-r14d
x86-r15d

x86-rax
x86-rcx
x86-rdx
x86-rbx
x86-rsp
x86-rbp
x86-rsi
x86-rdi
x86-r8
x86-r9
x86-r10
x86-r11
x86-r12
x86-r13
x86-r14
x86-r15

x86-st
x86-st1
x86-st2
x86-st3
x86-st4
x86-st5
x86-st6
x86-st7

x86-mm0
x86-mm1
x86-mm2
x86-mm3
x86-mm4
x86-mm5
x86-mm6
x86-mm7

x86-xmm0
x86-xmm1
x86-xmm2
x86-xmm3
x86-xmm4
x86-xmm5
x86-xmm6
x86-xmm7
x86-xmm8
x86-xmm9
x86-xmm10
x86-xmm11
x86-xmm12
x86-xmm13
x86-xmm14
x86-xmm15

x86-es
x86-cs
x86-ss
x86-ds
x86-fs
x86-gs

x86-arch-set!
x86-64bit-mode?
x86-word-width

x86-imm?

x86-imm-int
x86-imm-int?
x86-imm-int-width
x86-imm-int-value

x86-imm-lbl
x86-imm-lbl?
x86-imm-lbl-offset
x86-imm-lbl-label

x86-imm-late
x86-imm-late?
x86-imm-late-width
x86-imm-late-handler

x86-imm-obj
x86-imm-obj?
x86-imm-obj-value

x86-imm-glo
x86-imm-glo?
x86-imm-glo-name

x86-mem
x86-mem?
x86-mem-offset
x86-mem-reg1
x86-mem-reg2
x86-mem-scale

x86-label
x86-db
x86-dw
x86-dd
x86-dq

x86-add
x86-or
x86-adc
x86-sbb
x86-and
x86-sub
x86-xor
x86-cmp
x86-mov

x86-inc
x86-dec

x86-lea

x86-ret

x86-enter

x86-nop
x86-leave
x86-hlt
x86-cmc
x86-clc
x86-stc
x86-cli
x86-sti
x86-cld
x86-std

x86-int
x86-int3

x86-syscall
x86-sysret
x86-wrmsr
x86-rdtsc
x86-rdmsr
x86-rdpmc
x86-cpuid

x86-jmp
x86-call
x86-jo
x86-jno
x86-jb
x86-jae
x86-je
x86-jne
x86-jbe
x86-ja
x86-js
x86-jns
x86-jp
x86-jnp
x86-jl
x86-jge
x86-jle
x86-jg

x86-push
x86-pop
x86-pushf
x86-popf

x86-cwde
x86-cdq
x86-cbw
x86-cwd
x86-cdqe
x86-cqo

x86-rol
x86-ror
x86-rcl
x86-rcr
x86-shl
x86-shr
x86-sar

x86-neg
x86-not

x86-test

x86-xchg

x86-mul
x86-imul
x86-div
x86-idiv

x86-movzx
x86-movsx

x86-bt
x86-bts
x86-btr
x86-btc

x86-cmova
x86-cmovae
x86-cmovb
x86-cmovbe
x86-cmovc
x86-cmove
x86-cmovg
x86-cmovge
x86-cmovl
x86-cmovle
x86-cmovna
x86-cmovnae
x86-cmovnb
x86-cmovnbe
x86-cmovnc
x86-cmovne
x86-cmovng
x86-cmovnge
x86-cmovnl
x86-cmovnle
x86-cmovno
x86-cmovnp
x86-cmovns
x86-cmovnz
x86-cmovo
x86-cmovp
x86-cmovpe
x86-cmovpo
x86-cmovs
x86-cmovz

x86-popcnt
x86-lzcnt

))

;; Define x86 register classes.

(define-macro (x86-define-registers . definitions)

  (define names (make-vector (+ 96 8) "invalidreg"))

  (define (get d attrib)
    (let ((x (member attrib (cdr d))))
      (if x (cadr x) #f)))

  (define (gen-def d)
    (let ((id (car d)))
      (let ((class (get d 'class:))
            (field (get d 'field:))
            (mode  (get d 'mode:))
            (name  (or (get d 'name:) id)))
        (if (member class '(r8 r16 r32 r64 fpu mm xmm))
            (let ((i (+ field
                        (if (and (eq? class 'r8)
                                 (>= field 4)
                                 (< field 8)
                                 (not (eq? mode 'long)))
                            16
                            0)
                        (case class
                          ((r64) 0)
                          ((r32) 16)
                          ((r16) 32)
                          ((fpu) 48)
                          ((mm)  56)
                          ((xmm) 64)
                          ((r8)  80)))))
              (vector-set! names i name)
              `((define-macro (,(string->symbol (string-append "x86-" (symbol->string id))))
                  ,i)))
            `()))))

  (let* ((defs
           (apply append (map gen-def definitions)))
         (code
          `(begin
             (define-macro (x86-implement)
               `(begin
                  (define (x86-register-name reg)
                    (vector-ref ',',names reg))))
             (define-macro (x86-reg? x)
               `(fixnum? ,x))
             (define-macro (x86-reg8? reg)
               `(let ((n ,reg)) (fx>= n 80)))
             (define-macro (x86-reg8-h? reg)
               `(let ((n ,reg)) (fx>= n 96)))
             (define-macro (x86-xmm? reg)
               `(let ((n ,reg)) (and (fx>= n 64) (fx< n 80))))
             (define-macro (x86-mm? reg)
               `(let ((n ,reg)) (and (fx>= n 56) (fx< n 64))))
             (define-macro (x86-fpu? reg)
               `(let ((n ,reg)) (and (fx>= n 48) (fx< n 56))))
             (define-macro (x86-reg16? reg)
               `(let ((n ,reg)) (and (fx>= n 32) (fx< n 48))))
             (define-macro (x86-reg32? reg)
               `(let ((n ,reg)) (and (fx>= n 16) (fx< n 32))))
             (define-macro (x86-reg64? reg)
               `(let ((n ,reg)) (fx< n 16)))
             (define-macro (x86-reg-field reg)
               `(fxand ,reg 15))
             (define-macro (x86-reg8 n)
               `(fx+ 80 ,n))
             (define-macro (x86-reg16 n)
               `(fx+ 32 ,n))
             (define-macro (x86-reg32 n)
               `(fx+ 16 ,n))
             (define-macro (x86-reg64 n)
               n)
             (define-macro (x86-fpu n)
               `(fx+ 48 ,n))
             (define-macro (x86-reg-width reg)
               `(let ((n ,reg))
                  (cond ((fx< n 16) 64)
                        ((fx< n 32) 32)
                        ((fx< n 48) 16)
                        ((fx< n 64) 80)
                        ((fx< n 80) 128)
                        (else       8))))
             ,@defs)))
    ;;(pp code)
    ;;(pp names)
    code))

(x86-define-registers

  (al      class: r8  field: 0 )
  (cl      class: r8  field: 1 )
  (dl      class: r8  field: 2 )
  (bl      class: r8  field: 3 )
  (ah      class: r8  field: 4 )
  (ch      class: r8  field: 5 )
  (dh      class: r8  field: 6 )
  (bh      class: r8  field: 7 )
  (spl     class: r8  field: 4  mode: long)
  (bpl     class: r8  field: 5  mode: long)
  (sil     class: r8  field: 6  mode: long)
  (dil     class: r8  field: 7  mode: long)
  (r8b     class: r8  field: 8  mode: long)
  (r9b     class: r8  field: 9  mode: long)
  (r10b    class: r8  field: 10 mode: long)
  (r11b    class: r8  field: 11 mode: long)
  (r12b    class: r8  field: 12 mode: long)
  (r13b    class: r8  field: 13 mode: long)
  (r14b    class: r8  field: 14 mode: long)
  (r15b    class: r8  field: 15 mode: long)

  (ax      class: r16 field: 0 )
  (cx      class: r16 field: 1 )
  (dx      class: r16 field: 2 )
  (bx      class: r16 field: 3 )
  (sp      class: r16 field: 4 )
  (bp      class: r16 field: 5 )
  (si      class: r16 field: 6 )
  (di      class: r16 field: 7 )
  (r8w     class: r16 field: 8  mode: long)
  (r9w     class: r16 field: 9  mode: long)
  (r10w    class: r16 field: 10 mode: long)
  (r11w    class: r16 field: 11 mode: long)
  (r12w    class: r16 field: 12 mode: long)
  (r13w    class: r16 field: 13 mode: long)
  (r14w    class: r16 field: 14 mode: long)
  (r15w    class: r16 field: 15 mode: long)

  (eax     class: r32 field: 0 )
  (ecx     class: r32 field: 1 )
  (edx     class: r32 field: 2 )
  (ebx     class: r32 field: 3 )
  (esp     class: r32 field: 4 )
  (ebp     class: r32 field: 5 )
  (esi     class: r32 field: 6 )
  (edi     class: r32 field: 7 )
  (r8d     class: r32 field: 8  mode: long)
  (r9d     class: r32 field: 9  mode: long)
  (r10d    class: r32 field: 10 mode: long)
  (r11d    class: r32 field: 11 mode: long)
  (r12d    class: r32 field: 12 mode: long)
  (r13d    class: r32 field: 13 mode: long)
  (r14d    class: r32 field: 14 mode: long)
  (r15d    class: r32 field: 15 mode: long)

  (rax     class: r64 field: 0 )
  (rcx     class: r64 field: 1 )
  (rdx     class: r64 field: 2 )
  (rbx     class: r64 field: 3 )
  (rsp     class: r64 field: 4 )
  (rbp     class: r64 field: 5 )
  (rsi     class: r64 field: 6 )
  (rdi     class: r64 field: 7 )
  (r8      class: r64 field: 8  mode: long)
  (r9      class: r64 field: 9  mode: long)
  (r10     class: r64 field: 10 mode: long)
  (r11     class: r64 field: 11 mode: long)
  (r12     class: r64 field: 12 mode: long)
  (r13     class: r64 field: 13 mode: long)
  (r14     class: r64 field: 14 mode: long)
  (r15     class: r64 field: 15 mode: long)

  (st      class: fpu field: 0 )
  (st1     class: fpu field: 1 name: |st(1)|)
  (st2     class: fpu field: 2 name: |st(2)|)
  (st3     class: fpu field: 3 name: |st(3)|)
  (st4     class: fpu field: 4 name: |st(4)|)
  (st5     class: fpu field: 5 name: |st(5)|)
  (st6     class: fpu field: 6 name: |st(6)|)
  (st7     class: fpu field: 7 name: |st(7)|)

  (mm0     class: mm  field: 0 )
  (mm1     class: mm  field: 1 )
  (mm2     class: mm  field: 2 )
  (mm3     class: mm  field: 3 )
  (mm4     class: mm  field: 4 )
  (mm5     class: mm  field: 5 )
  (mm6     class: mm  field: 6 )
  (mm7     class: mm  field: 7 )

  (xmm0    class: xmm field: 0 )
  (xmm1    class: xmm field: 1 )
  (xmm2    class: xmm field: 2 )
  (xmm3    class: xmm field: 3 )
  (xmm4    class: xmm field: 4 )
  (xmm5    class: xmm field: 5 )
  (xmm6    class: xmm field: 6 )
  (xmm7    class: xmm field: 7 )
  (xmm8    class: xmm field: 8  mode: long)
  (xmm9    class: xmm field: 9  mode: long)
  (xmm10   class: xmm field: 10 mode: long)
  (xmm11   class: xmm field: 11 mode: long)
  (xmm12   class: xmm field: 12 mode: long)
  (xmm13   class: xmm field: 13 mode: long)
  (xmm14   class: xmm field: 14 mode: long)
  (xmm15   class: xmm field: 15 mode: long)

  (es      class: seg field: 0 )
  (cs      class: seg field: 1 )
  (ss      class: seg field: 2 )
  (ds      class: seg field: 3 )
  (fs      class: seg field: 4 )
  (gs      class: seg field: 5 )

)

;;;----------------------------------------------------------------------------

;; Create a new code generation context.  The format of the resulting
;; assembly code listing can also be specified, either 'nasm, 'gnu, or
;; #f (no listing, which is the default).

(define (make-cgc #!optional (listing-format #f))
  (let ((cgc (make-codegen-context)))
    (asm-init-code-block cgc 0 endianness)
    (codegen-context-listing-format-set! cgc listing-format)
    (x86-arch-set! cgc arch)
    cgc))

(define (asm gen #!optional (listing-format 'nasm))
  (let ((cgc (make-cgc listing-format)))

    (gen cgc)

    (let* ((code (asm-assemble-to-u8vector cgc))
           (fixup-locs (codegen-context-fixup-locs->vector cgc))
           (fixup-objs (codegen-context-fixup-objs->vector cgc)))
      (if listing-format
          (asm-display-listing cgc (current-error-port) #t))
      (u8vector->procedure code fixup-locs fixup-objs))))

;;;----------------------------------------------------------------------------

(define (test)

  (define add
    (asm
     (lambda (cgc)

       ;; x86-32 C parameter passing convention:
       ;;
       ;;  0(esp) is return address
       ;;  4(esp) is arg1
       ;;  8(esp) is arg2
       ;; 12(esp) is arg3
       ;;     eax is used to return the result

       ;; x86-64 C parameter passing convention:
       ;;
       ;;  0(rsp) is return address
       ;;     rdi is arg1
       ;;     rsi is arg2
       ;;     rdx is arg3
       ;;     rax is used to return the result

       (case arch

         ((x86-32)
          (x86-mov cgc (x86-eax) (x86-mem 4 (x86-esp))) ;; move arg1 to eax
          (x86-add cgc (x86-eax) (x86-mem 8 (x86-esp))) ;; add arg2 to eax
          )

         ((x86-64)
          (x86-mov cgc (x86-rax) (x86-rdi)) ;; move arg1 to rax
          (x86-add cgc (x86-rax) (x86-rsi)) ;; add arg2 to rax
          ))

       (x86-ret cgc))

     'nasm))

  (println "CPU architecture is " arch)
  (println)

  (println "(add 1 2) = " (add 1 2)))

;;;----------------------------------------------------------------------------

;; Redefine compile-file-to-target so that it immediately executes the
;; compiled code instead of generating a .oN file.

(define (compile-file-to-target
         filename
         #!key
         (options '())
         (output "")
         (expression #f))

  ;; The CPU backend will call compile-file-to-target with an
  ;; expression parameter of the form:
  ;;
  ;;   ((##machine-code-fixup <code>         ;; u8vector
  ;;                          <fixup-locs>   ;; u8vector
  ;;                          <fixup-objs>)) ;; vector

  (if (pair? expression)
      (eval expression)
      (##compile-file-to-target filename options output)))

(define (compile-and-load path)
  (compile-file-to-target path
                          options: '((target x86)
                                     ;;(expansion)
                                     ;;(verbose)
                                     )))

;;;----------------------------------------------------------------------------

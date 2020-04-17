; Mimosa
; Université de Montréal
; Marc Feeley, Samuel Yvon
;;;----------------------------------------------------

;; Import functions for generating x86 machine code.

(include "~~lib/_asm#.scm")
(include "~~lib/_x86#.scm")
(include "~~lib/_codegen#.scm")

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

(define (asm gen #!optional (format 'gnu))
  (let ((cgc (make-cgc format)))

    (gen cgc)

    (let ((code (asm-assemble-to-u8vector cgc)))

      (if format
          (asm-display-listing cgc (current-error-port) #t))

      (u8vector->procedure code))))

;;;----------------------------------------------------

(println "==> creating f5:")

(define f5
  (asm
   (lambda (cgc)

     (x86-mov cgc (x86-eax) (x86-imm-int (* 5 4)))
     (x86-ret cgc)  ;; return Scheme value 5 in rax

     )))

(println "==> creating nth:")

(define nth
  (asm
   (lambda (cgc)

     (define loop (asm-make-label cgc 'loop))
     (define test (asm-make-label cgc 'test))
     (define lst  (x86-edi))
     (define i    (x86-esi))
     (define (getcar x) (x86-mem (+ 5 -8) x))
     (define (getcdr x) (x86-mem (+ 1 -8) x))

     (x86-mov   cgc lst (x86-mem 4 (x86-esp)))
     (x86-mov   cgc i (x86-mem 8 (x86-esp)))
     (x86-cmp   cgc i (x86-imm-int 0))
     (x86-jmp   cgc test)

     (x86-label cgc loop)
     (x86-mov   cgc lst (getcdr lst))
     (x86-sub   cgc i (x86-imm-int 4))

     (x86-label cgc test)
     (x86-jne   cgc loop)

     (x86-mov   cgc (x86-eax) (getcar lst))
     (x86-ret   cgc)  ;; return result in rax

     )))

(pp (nth '(100 101 102 103 104 105 106 107)
         (f5)))

;;;----------------------------------------------------

#|
x86 C parameter passing convention:

 x86-32  x86-64
 0(esp)  0(rsp)  return addr
 4(esp)     rdi  arg1
 8(esp)     rsi  arg2
12(esp)     rdx  arg3
    eax     rax  fn result
|#

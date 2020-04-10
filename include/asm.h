// file: "asm.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

#ifndef __ASM_H
#define __ASM_H

//-----------------------------------------------------------------------------

#include "general.h"

//-----------------------------------------------------------------------------

// CPU interrupt enable/disable.

#define disable_interrupts() __asm__ __volatile__("cli" : : : "memory")
#define enable_interrupts() __asm__ __volatile__("sti" : : : "memory")
#define halt() __asm__ __volatile__("hlt" : : : "memory")

//-----------------------------------------------------------------------------

// Access the CPU's flags and the code segment register.

#define eflags_reg()                                                           \
  ({                                                                           \
    uint32 val;                                                                \
    __asm__ __volatile__("pushfl;popl %0" : "=q"(val));                        \
    val;                                                                       \
  })

#define cs_reg()                                                               \
  ({                                                                           \
    uint32 val;                                                                \
    __asm__ __volatile__("pushl %%cs;popl %0" : "=q"(val));                    \
    val;                                                                       \
  })

#define cr4_reg()                                                              \
  ({                                                                           \
    uint32 val;                                                                \
    __asm__ __volatile__("movl %%cr4,%0" : "=g"(val));                         \
    val;                                                                       \
  })

//-----------------------------------------------------------------------------

// Access to the time stamp counter and performance monitoring counters.

#define cpuid(fn, a, b, c, d)                                                  \
  __asm__ __volatile__(".byte 0x0f,0xa2"                                       \
                       : "=a"(a), "=b"(b), "=c"(c), "=d"(d)                    \
                       : "0"(fn))

#define HAS_FPU (1 << 0)        // Floating Point Unit
#define HAS_VME (1 << 1)        // V86 Mode Extensions
#define HAS_DE (1 << 2)         // Debug Extensions
#define HAS_PSE (1 << 3)        // Page Size Extensions
#define HAS_TSC (1 << 4)        // Time Stamp Counter
#define HAS_MSR (1 << 5)        // Model Specific Registers
#define HAS_PAE (1 << 6)        // Physical Address Extensions
#define HAS_MCE (1 << 7)        // Machine Check Exception
#define HAS_CX8 (1 << 8)        // CMPXCHG8B instruction
#define HAS_APIC (1 << 9)       // Local APIC present
#define HAS_SEP (1 << 11)       // Fast system call
#define HAS_MTRR (1 << 12)      // Memory Type Range Registers
#define HAS_PGE (1 << 13)       // Page Global Enable
#define HAS_MCA (1 << 14)       // Machine Check Architecture
#define HAS_CMOV (1 << 15)      // Conditional MOVe
#define HAS_PAT (1 << 16)       // Page Attribute Table
#define HAS_PSE36 (1 << 17)     // 36 bit Page Size Extensions
#define HAS_PSN (1 << 18)       // Processor Serial Number
#define HAS_CFLSH (1 << 19)     // Cache Flush
#define HAS_DTES (1 << 21)      // Debug Trace Store
#define HAS_ACPI (1 << 22)      // ACPI support
#define HAS_MMX (1 << 23)       // MultiMedia Extensions
#define HAS_FXSR (1 << 24)      // FXSAVE and FXRSTOR
#define HAS_SSE (1 << 25)       // SSE instructions
#define HAS_SSE2 (1 << 26)      // SSE2 instructions
#define HAS_SELFSNOOP (1 << 27) // Self Snoop
#define HAS_ACC (1 << 29)       // Automatic clock control
#define HAS_IA64 (1 << 30)      // IA64 instructions

#define wrmsr(msr, val)                                                        \
  __asm__ __volatile__(".byte 0x0f,0x30" : : "A"(CAST(uint64, val)), "c"(msr))

#define rdtsc()                                                                \
  ({                                                                           \
    int64 val;                                                                 \
    __asm__ __volatile__(".byte 0x0f,0x31" : "=A"(val));                       \
    val;                                                                       \
  })

#define rdmsr(msr)                                                             \
  ({                                                                           \
    int64 val;                                                                 \
    __asm__ __volatile__(".byte 0x0f,0x32" : "=A"(val) : "c"(msr));            \
    val;                                                                       \
  })

#define rdpmc(pmc)                                                             \
  ({                                                                           \
    int64 val;                                                                 \
    __asm__ __volatile__(".byte 0x0f,0x33" : "=A"(val) : "c"(pmc));            \
    val;                                                                       \
  })

#define MSR_MCA_REG 0
#define MSR_MCT_REG 1
#define MSR_TR1_REG 2
#define MSR_TR2_REG 4
#define MSR_TR3_REG 5
#define MSR_TR4_REG 6
#define MSR_TR5_REG 7
#define MSR_TR6_REG 8
#define MSR_TR7_REG 9
#define MSR_TR9_REG 11
#define MSR_TR10_REG 12
#define MSR_TR11_REG 13
#define MSR_TR12_REG 14
#define MSR_TSC_REG 16
#define MSR_CES_REG 17
#define MSR_CTR0_REG 18
#define MSR_CTR1_REG 19

#define CES_CONFIG(ctr0, ctr1) (((ctr1) << 16) + (ctr0))
#define CES_SETTING(es, cc, pc) (((pc) << 9) + ((cc) << 6) + (es))

#define CES_ES_DATA_READ 0
#define CES_ES_DATA_WRITE 1
#define CES_ES_DATA_TLB_MISS 2
#define CES_ES_DATA_READ_MISS 3
#define CES_ES_DATA_WRITE_MISS 4
#define CES_ES_CODE_READ 12
#define CES_ES_CODE_TLB_MISS 13
#define CES_ES_CODE_READ_MISS 14
#define CES_ES_BRANCHES 18
#define CES_ES_FLOPS 34
#define CES_ES_HARDWARE_INTS 39
#define CES_ES_DATA_READ_OR_WRITE 40
#define CES_ES_DATA_READ_OR_WRITE_MISS 41
#define CES_ES_BRANCHES_TAKEN 50

#define CES_CC_DISABLE 0
#define CES_CC_COUNT_CPL012 1
#define CES_CC_COUNT_CPL3 2
#define CES_CC_COUNT 3
#define CES_CC_CLOCKS_CPL012 5
#define CES_CC_CLOCKS_CPL3 6
#define CES_CC_CLOCKS 7

#define CES_PC_INCREMENT 0
#define CES_PC_OVERFLOW 1

//-----------------------------------------------------------------------------

// Write-back and invalidate cache.

#define wbinvd() __asm__ __volatile__("wbinvd")

//-----------------------------------------------------------------------------

// Access to the I/O ports.

// The following macro definitions to access the x86 I/O instructions
// have been taken from the linux kernel to ensure that the operations
// to access the I/O ports are compatible with linux.  This may allow
// easier porting of linux device drivers to this OS.

#ifdef SLOW_IO_BY_JUMPING
#define __SLOW_DOWN_IO __asm__ __volatile__("jmp 1f\n1:\tjmp 1f\n1:")
#else
#define __SLOW_DOWN_IO __asm__ __volatile__("outb %al,$0x80")
#endif

#ifdef REALLY_SLOW_IO
#define SLOW_DOWN_IO                                                           \
  {                                                                            \
    __SLOW_DOWN_IO;                                                            \
    __SLOW_DOWN_IO;                                                            \
    __SLOW_DOWN_IO;                                                            \
    __SLOW_DOWN_IO;                                                            \
  }
#else
#define SLOW_DOWN_IO __SLOW_DOWN_IO
#endif

// readX/writeX() are used to access memory mapped devices. On some
// architectures the memory mapped IO stuff needs to be accessed
// differently. On the x86 architecture, we just read/write the
// memory location directly.

#define readb(addr) (*(volatile unsigned char *)(addr))
#define readw(addr) (*(volatile unsigned short *)(addr))
#define readl(addr) (*(volatile unsigned int *)(addr))

#define writeb(b, addr) ((*(volatile unsigned char *)(addr)) = (b))
#define writew(b, addr) ((*(volatile unsigned short *)(addr)) = (b))
#define writel(b, addr) ((*(volatile unsigned int *)(addr)) = (b))

#define memset_io(a, b, c) memset((void *)(a), (b), (c))
#define memcpy_fromio(a, b, c) memcpy((a), (void *)(b), (c))
#define memcpy_toio(a, b, c) memcpy((void *)(a), (b), (c))

// Again, i386 does not require mem IO specific function.

#define eth_io_copy_and_sum(a, b, c, d)                                        \
  eth_copy_and_sum((a), (void *)(b), (c), (d))

// Talk about misusing macros..

#define __OUT1(s, x)                                                           \
  extern void __out##s(unsigned x value, unsigned short port);                 \
  extern inline void __out##s(unsigned x value, unsigned short port) {

#define __OUT2(s, s1, s2)                                                      \
__asm__ __volatile__ ("out" #s " %" s1 "0,%" s2 "1"

#define __OUT(s, s1, x)                                                        \
__OUT1(s,x) __OUT2(s,s1,"w") : : "a" (value), "d" (port));                     \
  }                                                                            \
__OUT1(s##c,x) __OUT2(s,s1,"") : : "a" (value), "id" (port));                  \
  }                                                                            \
__OUT1(s##_p,x) __OUT2(s,s1,"w") : : "a" (value), "d" (port));                 \
  SLOW_DOWN_IO;                                                                \
  }                                                                            \
__OUT1(s##c_p,x) __OUT2(s,s1,"") : : "a" (value), "id" (port));                \
  SLOW_DOWN_IO;                                                                \
  }

#define __IN1(s, x)                                                            \
  extern unsigned x __in##s(unsigned short port);                              \
  extern inline unsigned x __in##s(unsigned short port) {                      \
    unsigned x _v;

#define __IN2(s, s1, s2)                                                       \
__asm__ __volatile__ ("in" #s " %" s2 "1,%" s1 "0"

#define __IN(s, s1, x, i...)                                                   \
__IN1(s,x) __IN2(s,s1,"w") : "=a" (_v) : "d" (port) ,##i );                    \
  return _v;                                                                   \
  }                                                                            \
__IN1(s##c,x) __IN2(s,s1,"") : "=a" (_v) : "id" (port) ,##i );                 \
  return _v;                                                                   \
  }                                                                            \
__IN1(s##_p,x) __IN2(s,s1,"w") : "=a" (_v) : "d" (port) ,##i );                \
  SLOW_DOWN_IO;                                                                \
  return _v;                                                                   \
  }                                                                            \
__IN1(s##c_p,x) __IN2(s,s1,"") : "=a" (_v) : "id" (port) ,##i );               \
  SLOW_DOWN_IO;                                                                \
  return _v;                                                                   \
  }

#define __INS(s)                                                               \
  extern void ins##s(unsigned short port, void *addr, unsigned long count);    \
  extern inline void ins##s(unsigned short port, void *addr,                   \
                            unsigned long count) {                             \
    __asm__ __volatile__("cld ; rep ; ins" #s                                  \
                         : "=D"(addr), "=c"(count)                             \
                         : "d"(port), "0"(addr), "1"(count));                  \
  }

#define __OUTS(s)                                                              \
  extern void outs##s(unsigned short port, const void *addr,                   \
                      unsigned long count);                                    \
  extern inline void outs##s(unsigned short port, const void *addr,            \
                             unsigned long count) {                            \
    __asm__ __volatile__("cld ; rep ; outs" #s                                 \
                         : "=S"(addr), "=c"(count)                             \
                         : "d"(port), "0"(addr), "1"(count));                  \
  }

__IN(b, "", char)
__IN(w, "", short)
__IN(l, "", long)

__OUT(b, "b", char)
__OUT(w, "w", short)
__OUT(l, , int)

__INS(b)
__INS(w)
__INS(l)

__OUTS(b)
__OUTS(w)
__OUTS(l)

// Note that due to the way __builtin_constant_p() works, you
//  - can't use it inside a inline function (it will never be true)
//  - you don't have to worry about side effects within the __builtin..

#define outb(val, port)                                                        \
  ((__builtin_constant_p((port)) && (port) < 256) ? __outbc((val), (port))     \
                                                  : __outb((val), (port)))

#define inb(port)                                                              \
  ((__builtin_constant_p((port)) && (port) < 256) ? __inbc(port) : __inb(port))

#define outb_p(val, port)                                                      \
  ((__builtin_constant_p((port)) && (port) < 256) ? __outbc_p((val), (port))   \
                                                  : __outb_p((val), (port)))

#define inb_p(port)                                                            \
  ((__builtin_constant_p((port)) && (port) < 256) ? __inbc_p(port)             \
                                                  : __inb_p(port))

#define outw(val, port)                                                        \
  ((__builtin_constant_p((port)) && (port) < 256) ? __outwc((val), (port))     \
                                                  : __outw((val), (port)))

#define inw(port)                                                              \
  ((__builtin_constant_p((port)) && (port) < 256) ? __inwc(port) : __inw(port))

#define outw_p(val, port)                                                      \
  ((__builtin_constant_p((port)) && (port) < 256) ? __outwc_p((val), (port))   \
                                                  : __outw_p((val), (port)))

#define inw_p(port)                                                            \
  ((__builtin_constant_p((port)) && (port) < 256) ? __inwc_p(port)             \
                                                  : __inw_p(port))

#define outl(val, port)                                                        \
  ((__builtin_constant_p((port)) && (port) < 256) ? __outlc((val), (port))     \
                                                  : __outl((val), (port)))

#define inl(port)                                                              \
  ((__builtin_constant_p((port)) && (port) < 256) ? __inlc(port) : __inl(port))

#define outl_p(val, port)                                                      \
  ((__builtin_constant_p((port)) && (port) < 256) ? __outlc_p((val), (port))   \
                                                  : __outl_p((val), (port)))

#define inl_p(port)                                                            \
  ((__builtin_constant_p((port)) && (port) < 256) ? __inlc_p(port)             \
                                                  : __inl_p(port))

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //

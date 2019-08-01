# file: "kernel.s"

# Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
# Rights Reserved.
#
# Revision History
# 22 Sep 01  initial version (Marc Feeley)
#------------------------------------------------------------------------------

  .globl kernel_entry

kernel_entry:  # this is the kernel's entry point

  # Note: the second stage / kernel is loaded by the boot sector at "KERNEL_START".

  .code16  # at this point the processor is in 16 bit real mode
           # and %cs is equal to (KERNEL_START>>4)

set_video_mode:

# Setup an appropriate video mode.

# Try to find a high resolution VESA mode using VBE 1.2 predefined modes
# with 16M colors.

  movl  $(modes-kernel_entry+KERNEL_START),0x4444

  movw  $0x11b,%cx  # 1280x1024 16M colors
  call  check_video_mode

  movw  $0x118,%cx  # 1024x768 16M colors
  call  check_video_mode

  movw  $0x115,%cx  # 800x600 16M colors
  call  check_video_mode

  movw  $0x112,%cx  # 640x480 16M colors
  call  check_video_mode

  movw  $0x10f,%cx  # 320x200 16M colors
  call  check_video_mode

  movw  $((vbe_info-kernel_entry+KERNEL_START)>>4)&0xffff,%ax
  movw  %ax,%es
  movw  $(vbe_info-kernel_entry+KERNEL_START)&0xf,%di
  movb  $'V',%es:(%di)
  movb  $'B',%es:1(%di)
  movb  $'E',%es:2(%di)
  movb  $'2',%es:3(%di)
  movw  $0x4f00,%ax # select BIOS function: "Return VBE Controller Information"
  int   $0x10  # call BIOS

  movl  %es:(VideoModePtr-vbe_info)(%di),%eax
  xorl  %edx,%edx
  movw  %ax,%dx
  shrl  $12,%eax
  andw  $0xfff0,%ax
  addl  %eax,%edx

check_next_video_mode:

  movl  %edx,%ebx
  shrl  $4,%ebx
  movw  %bx,%es
  movw  %dx,%bx
  andw  $0xf,%bx

  movw  %es:(%bx),%cx
  addl  $2,%edx

  cmpw  $0xffff,%cx
  je    all_video_modes_tried

  call  check_video_mode

  jmp   check_next_video_mode

check_video_mode:

  movw  $((mode_info-kernel_entry+KERNEL_START)>>4)&0xffff,%ax
  movw  %ax,%es
  movw  $(mode_info-kernel_entry+KERNEL_START)&0xf,%di
  movw  $0x4f01,%ax # select BIOS function: "Return VBE Mode Information"
  int   $0x10  # call BIOS

  cmpw  $0x4f,%ax
  jne   check_video_mode_return

  movw  %es:(ModeAttributes-mode_info)(%di),%ax
  andw  $(1<<7)+(1<<4)+(1<<3)+(1<<0),%ax
  cmpw  $(1<<7)+(1<<4)+(1<<3)+(1<<0),%ax
  jne   check_video_mode_return

  cmpb  $32,%es:(BitsPerPixel-mode_info)(%di)
  jne   check_video_mode_return

  movw  %es:(YResolution-mode_info)(%di),%ax
  shll  $16,%eax
  movw  %es:(XResolution-mode_info)(%di),%ax
  shll  $16,%ecx
  movb  %es:(BitsPerPixel-mode_info)(%di),%cl

  movl  0x4444,%ebx
  shrl  $4,%ebx
  movw  %bx,%es
  movl  0x4444,%ebx
  andw  $0xf,%bx

  movl  %eax,%es:(%bx)
  movl  %ecx,%es:4(%bx)
  movl  $0,%es:8(%bx)
  movl  0x4444,%ebx
  addl  $8,%ebx
  movl  %ebx,0x4444

check_video_mode_return:

  ret

  movw  $0x4f02,%ax # select BIOS function: "SET SuperVGA VIDEO MODE"
  movw  $0x12b+(1<<14),%bx   # mode = 1024x768x16M
  int   $0x10  # call BIOS

all_video_modes_tried:

# Set video mode to mode 18 (graphic 640x480 16 colors, fb at 0xa0000).
# This has to be done while we are still in 16 bit real mode, because that's
# what the BIOS expects.

  movw  $(0<<8)+18,%ax
  int   $0x10  # call BIOS

#------------------------------------------------------------------------------

# Physical memory map:
#
#            +------------+
# 0xffffffff |     .      |
#            | kernel heap|
#            |     .      |
# 0x01000000 +------------+
#            |     .      |
#            |   Gambit   |
# 0x00100000 |     .      |
#            +------------+
#            |     .      |
#            |  BIOS ROM  |
# 0x000c0000 |     .      |
#            +------------+
#            |     .      |
#            | video card |
# 0x000a0000 |     .      |
#            +------------+
#            |     .      |
#            |   kernel   |
# 0x00020000 |     .      |
#            +------------+
#            | global     |
#            | descriptor |
# 0x00010000 | table      | <---- GDTR (Global Descriptor Table Register)
#            +------------+
#            | scratch    |
#            | area for   |
# 0x00001000 | stack, etc |
#            +------------+
#            | interrupt  |
#            | descriptor |
# 0x00000800 | table      | <---- IDTR (Interrupt Descriptor Table Register)
#            +------------+
#            |     .      |
#            |     .      |
# 0x00000400 |     .      |
#            +------------+
#            | real mode  | the real mode interrupt vector table is not
#            | interrupt  | overwritten, just in case we need to switch
#            | vector     | back to real mode
# 0x00000000 | table      |
#            +------------+

SCRATCH_BOT        = 0x1000
SCRATCH_TOP        = 0x10000
PAGE_DIR           = 0x1000  # physical address, must be multiple of 4096

INTR_DESCR_TABLE   = 0x800   # physical address
NB_INTR_DESCRS     = 0x100

GLOBAL_DESCR_TABLE = 0x10000
NB_GLOBAL_DESCRS   = 0x2000

LOCAL_DESCR_TABLE  = 0x20000
NB_LOCAL_DESCRS    = 0x2000

NOT_PRESENT = 0
PRESENT = 1

GATE_16 = 0
GATE_32 = 1

GRANU_1    = 0
GRANU_4096 = 1

DEF_SIZE_16 = 0
DEF_SIZE_32 = 1

SP_SIZE_16 = 0
SP_SIZE_32 = 1

DATA_SEG_RO                = (16+0)
DATA_SEG_RO_ACCESSED       = (16+1)
DATA_SEG_RW                = (16+2)
DATA_SEG_RW_ACCESSED       = (16+3)
DATA_SEG_RO_XDOWN          = (16+4)
DATA_SEG_RO_XDOWN_ACCESSED = (16+5)
DATA_SEG_RW_XDOWN          = (16+6)
DATA_SEG_RW_XDOWN_ACCESSED = (16+7)
CODE_SEG_EO                = (16+8)
CODE_SEG_EO_ACCESSED       = (16+9)
CODE_SEG_ER                = (16+10)
CODE_SEG_ER_ACCESSED       = (16+11)
CODE_SEG_EO_CONF           = (16+12)
CODE_SEG_EO_CONF_ACCESSED  = (16+13)
CODE_SEG_ER_CONF           = (16+14)
CODE_SEG_ER_CONF_ACCESSED  = (16+15)

# When the processor is in protected mode, the segment registers
# contain segment selectors.
#
# segment selectors are 16 bits wide and have the following format:
#
#   1 1 1 1 1 1
#   5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  |segment index in GDT/LDT |T|RPL|
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  T (table indicator) T=0 for GDT, T=1 for LDT
#  RPL (requested privilege level) RPL=0 is highest level

NULL_SEGMENT = 0
SYST_SEGMENT = 1
CODE_SEGMENT = 2
DATA_SEGMENT = 3
NB_SEGMENTS  = 4

GDT = 0
LDT = 1

PL_0 = 0
PL_1 = 1
PL_2 = 2
PL_3 = 3

NULL_SEG_SEL = (NULL_SEGMENT<<3)+(GDT<<2)+PL_0
SYST_SEG_SEL = (SYST_SEGMENT<<3)+(GDT<<2)+PL_0
DATA_SEG_SEL = (DATA_SEGMENT<<3)+(GDT<<2)+PL_0
CODE_SEG_SEL = (CODE_SEGMENT<<3)+(GDT<<2)+PL_0

#------------------------------------------------------------------------------

init_segs:

# Initialize segment registers and stack pointer.

REAL_MODE_DATA_SEG = (KERNEL_START>>4)
REAL_MODE_CODE_SEG = (KERNEL_START>>4)
REAL_MODE_STACK_SEG = 0

  cli  # inhibit interrupts (the C part of the kernel will execute a "sti")

  movw  $SCRATCH_TOP-(REAL_MODE_STACK_SEG<<4),%sp
  movw  $REAL_MODE_STACK_SEG,%ax
  movw  %ax,%ss
  movw  $REAL_MODE_DATA_SEG,%ax
  movw  %ax,%ds

#------------------------------------------------------------------------------

init_idt:

# Initialize interrupt descriptor table.

# interrupt descriptors are 8 bytes long and have the following format:
#
#   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
#   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  |        offset 31..16          |P|DPL|0 D 1 1 0 0 0 0|X X X X X| +4
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  |       segment selector        |        offset 15..0           | +0
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  P (segment present flag) P=0 means ``not present'', P=1 means ``present''
#  DPL (descriptor privilege level) DPL=0 is highest level
#  D (size of gate) D=0 for 16 bits, D=1 for 32 bits

# initialize all IDT entries so that interrupts are ignored

  movl  $unhandled_intr,%ebx
  call  gen_intr_descr

  movw  $INTR_DESCR_TABLE>>4,%bp
  movw  %bp,%es
  movw  $NB_INTR_DESCRS*8,%bp
1:movl  %eax,%es:-8(%bp)
  movl  %ebx,%es:-4(%bp)
  subw  $8,%bp
  jne   1b

# initialize the IDT entries for INT0 to INT15, IRQ0 to IRQ15, and
# the APIC timer and spurious interrupts

INT0_INTR = 0

  movl  $int0_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT0_INTR+0
  movl  %ebx,%es:8*INT0_INTR+4

INT1_INTR = 1

  movl  $int1_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT1_INTR+0
  movl  %ebx,%es:8*INT1_INTR+4

INT2_INTR = 2

  movl  $int2_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT2_INTR+0
  movl  %ebx,%es:8*INT2_INTR+4

INT3_INTR = 3

  movl  $int3_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT3_INTR+0
  movl  %ebx,%es:8*INT3_INTR+4

INT4_INTR = 4

  movl  $int4_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT4_INTR+0
  movl  %ebx,%es:8*INT4_INTR+4

INT5_INTR = 5

  movl  $int5_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT5_INTR+0
  movl  %ebx,%es:8*INT5_INTR+4

INT6_INTR = 6

  movl  $int6_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT6_INTR+0
  movl  %ebx,%es:8*INT6_INTR+4

INT7_INTR = 7

  movl  $int7_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT7_INTR+0
  movl  %ebx,%es:8*INT7_INTR+4

INT8_INTR = 8

  movl  $int8_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT8_INTR+0
  movl  %ebx,%es:8*INT8_INTR+4

INT9_INTR = 9

  movl  $int9_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT9_INTR+0
  movl  %ebx,%es:8*INT9_INTR+4

INT10_INTR = 10

  movl  $int10_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT10_INTR+0
  movl  %ebx,%es:8*INT10_INTR+4

INT11_INTR = 11

  movl  $int11_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT11_INTR+0
  movl  %ebx,%es:8*INT11_INTR+4

INT12_INTR = 12

  movl  $int12_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT12_INTR+0
  movl  %ebx,%es:8*INT12_INTR+4

INT13_INTR = 13

  movl  $int13_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT13_INTR+0
  movl  %ebx,%es:8*INT13_INTR+4

INT14_INTR = 14

  movl  $int14_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT14_INTR+0
  movl  %ebx,%es:8*INT14_INTR+4

INT15_INTR = 15

  movl  $int15_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*INT15_INTR+0
  movl  %ebx,%es:8*INT15_INTR+4

IRQ0_INTR = 0xb0

  movl  $irq0_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ0_INTR+0
  movl  %ebx,%es:8*IRQ0_INTR+4

IRQ1_INTR = 0xb1

  movl  $irq1_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ1_INTR+0
  movl  %ebx,%es:8*IRQ1_INTR+4

IRQ2_INTR = 0xb2

  movl  $irq2_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ2_INTR+0
  movl  %ebx,%es:8*IRQ2_INTR+4

IRQ3_INTR = 0xb3

  movl  $irq3_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ3_INTR+0
  movl  %ebx,%es:8*IRQ3_INTR+4

IRQ4_INTR = 0xb4

  movl  $irq4_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ4_INTR+0
  movl  %ebx,%es:8*IRQ4_INTR+4

IRQ5_INTR = 0xb5

  movl  $irq5_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ5_INTR+0
  movl  %ebx,%es:8*IRQ5_INTR+4

IRQ6_INTR = 0xb6

  movl  $irq6_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ6_INTR+0
  movl  %ebx,%es:8*IRQ6_INTR+4

IRQ7_INTR = 0xb7

  movl  $irq7_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ7_INTR+0
  movl  %ebx,%es:8*IRQ7_INTR+4

IRQ8_INTR = 0xb8

  movl  $irq8_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ8_INTR+0
  movl  %ebx,%es:8*IRQ8_INTR+4

IRQ9_INTR = 0xb9

  movl  $irq9_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ9_INTR+0
  movl  %ebx,%es:8*IRQ9_INTR+4

IRQ10_INTR = 0xba

  movl  $irq10_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ10_INTR+0
  movl  %ebx,%es:8*IRQ10_INTR+4

IRQ11_INTR = 0xbb

  movl  $irq11_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ11_INTR+0
  movl  %ebx,%es:8*IRQ11_INTR+4

IRQ12_INTR = 0xbc

  movl  $irq12_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ12_INTR+0
  movl  %ebx,%es:8*IRQ12_INTR+4

IRQ13_INTR = 0xbd

  movl  $irq13_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ13_INTR+0
  movl  %ebx,%es:8*IRQ13_INTR+4

IRQ14_INTR = 0xbe

  movl  $irq14_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ14_INTR+0
  movl  %ebx,%es:8*IRQ14_INTR+4

IRQ15_INTR = 0xbf

  movl  $irq15_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*IRQ15_INTR+0
  movl  %ebx,%es:8*IRQ15_INTR+4

APIC_TIMER_INTR = 0xa0

  movl  $APIC_timer_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*APIC_TIMER_INTR+0
  movl  %ebx,%es:8*APIC_TIMER_INTR+4

APIC_SPURIOUS_INTR = 0xcf

  movl  $APIC_spurious_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*APIC_SPURIOUS_INTR+0
  movl  %ebx,%es:8*APIC_SPURIOUS_INTR+4

SYS_INTR = 0xd0

  movl  $sys_intr,%ebx
  call  gen_intr_descr
  movl  %eax,%es:8*SYS_INTR+0
  movl  %ebx,%es:8*SYS_INTR+4

  jmp   init_gdt

# Utility to generate interrupt descriptors.

gen_intr_descr:

  # Given the linear address of an interrupt handler in %ebx,
  # generates the two 32 bit parts of the interrupt descriptor in %eax
  # and %ebx.

  movl  %ebx,%eax
  roll  $16,%eax
  movw  $CODE_SEG_SEL,%ax
  roll  $16,%eax
  movw  $(PRESENT<<15)+(PL_0<<13)+(GATE_32<<11)+0x600,%bx

  ret

#------------------------------------------------------------------------------

init_gdt:

# Initialize global descriptor table.

# segment descriptors are 8 bytes long and have the following format:
#
#   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
#   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  | base address  |G|D|0|A| limit |P|DPL|S| TYPE  | base address  | +4
#  |    31..24     | |B| |V|19..16 | |   | |       |    23..16     |
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  |      base address 15..0       |         limit 15..0           | +0
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  G (limit granularity) G=0 for multiplier=1, G=1 for multiplier=4096
#  DB (default size; code segments) DB=0 for 16 bit, DB=1 for 32 bit
#  DB (big stack pointer; stack segments) DB=0 for %sp, DB=1 for %esp
#  AV (available for system software)
#  P (segment present flag) P=0 means ``not present'', P=1 means ``present''
#  DPL (descriptor privilege level) DPL=0 is highest level
#  S (descriptor type) S=0 for system, S=1 for code and data segments
#  TYPE (segment type)
#  note: when P=0 all fields except P, DPL, S and TYPE are available for
#  system software

# initialize all GDT entries as ``not-present''

  xorl  %eax,%eax
  movl  $(NOT_PRESENT<<15)+(PL_0<<13)+(DATA_SEG_RO<<8),%ebx

  movw  $GLOBAL_DESCR_TABLE>>4,%bp
  movw  %bp,%es
  movl  $NB_GLOBAL_DESCRS*8,%ebp
1:movl  %eax,%es:-8(%bp)
  movl  %ebx,%es:-4(%bp)
  subw  $8,%bp
  jne   1b

# initialize GDT entries for null segment, code segment and data segment

  xorl  %eax,%eax
  movl  %eax,%es:NULL_SEGMENT*8+0
  movl  %eax,%es:NULL_SEGMENT*8+4

  movl  $0x0000ffff,%es:CODE_SEGMENT*8+0
  movl  $(GRANU_4096<<23)+(DEF_SIZE_32<<22)+(PRESENT<<15)+(PL_0<<13)+(CODE_SEG_ER<<8)+0x000f0000,%es:CODE_SEGMENT*8+4

  movl  $0x0000ffff,%es:DATA_SEGMENT*8+0
  movl  $(GRANU_4096<<23)+(SP_SIZE_32<<22)+(PRESENT<<15)+(PL_0<<13)+(DATA_SEG_RW<<8)+0x000f0000,%es:DATA_SEGMENT*8+4

#------------------------------------------------------------------------------

init_paging:

# Initialize page-directory and page-tables.

# 4K page-directory entries are 4 bytes long and have the following format:
#
#  - for a page-table that is not present in memory:
#   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
#   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  |             available for system software                   |0|
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  - for a page-table that is present in memory:
#   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
#   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  |                                       |     | |P| | |P|P|U|R| |
#  |   page-table base physical address    |AVAIL|G|S|0|A|C|W|/|/|1|
#  |                                       |     | | | | |D|T|S|W| |
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  AVAIL (available for system software)
#  G (global page) ignored
#  PS (page size) PS=0 for 4KBytes, PS=1 for 4MBytes
#  A (accessed) set to 1 on every access to the page
#  PCD (page cache disabled) PCD=0 for caching, PCD=1 for no caching
#  PWT (page write-through) PWT=0 for write-back, PWT=1 for write-through
#  U/S (user/supervisor) U/S=0 for supervisor, U/S=1 for user
#  R/W (read/write) R/W=0 for read-only, R/W=1 for read/write permission
#
# 4K page-table entries are 4 bytes long and have the following format:
#
#  - for a page that is not present in memory:
#   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
#   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  |             available for system software                   |0|
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  - for a page that is present in memory:
#   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
#   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  |                                       |     | | | | |P|P|U|R| |
#  |      page base physical address       |AVAIL|G|0|D|A|C|W|/|/|1|
#  |                                       |     | | | | |D|T|S|W| |
#  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#  AVAIL (available for system software)
#  G (global page) G=0 for not global, G=1 for global
#  D (dirty) set to 1 on every write to the page
#  A (accessed) set to 1 on every access to the page
#  PCD (page cache disabled) PCD=0 for caching, PCD=1 for no caching
#  PWT (page write-through) PWT=0 for write-back, PWT=1 for write-through
#  U/S (user/supervisor) U/S=0 for supervisor, U/S=1 for user
#  R/W (read/write) R/W=0 for read-only, R/W=1 for read/write permission

# >>> Not yet implemented!

#------------------------------------------------------------------------------

switch_to_32bit_protected_mode:

# Switch to 32 bit protected mode.

  lidt  idtr_value-kernel_entry  # point IDTR to INT_DESCR_TABLE
  lgdt  gdtr_value-kernel_entry  # point GDTR to GLOBAL_DESCR_TABLE

  movl  %cr0,%eax  # turn on protected mode
  orb   $1,%al
  movl  %eax,%cr0

  jmp   prefetch_queue_cleared  # this jump clears the prefetch queue

idtr_value:
  .word NB_INTR_DESCRS*8-1    # limit
  .long INTR_DESCR_TABLE      # base

gdtr_value:
  .word NB_GLOBAL_DESCRS*8-1  # limit
  .long GLOBAL_DESCR_TABLE    # base

prefetch_queue_cleared:

  movw  $DATA_SEG_SEL,%ax  # set %ds, %es and %ss to the data segment selector
  movw  %ax,%ds
  movw  %ax,%es
  movw  %ax,%ss
  movl  $SCRATCH_TOP,%esp  # initialize stack pointer

  xorw  %ax,%ax  # make %fs and %gs unusable (null segment)
  movw  %ax,%fs
  movw  %ax,%gs

# We need to load %cs with the code segment selector by performing a
# "jump".  Because the "gas" assembler does not handle this instruction, we
# encode the instruction using the appropriate sequence of bytes.

  .byte 0x66  # operand-size = 32 bits
  .byte 0xea  # far jmp
  .long start_of_32bit_protected_mode  # destination address
  .word CODE_SEG_SEL                   # destination segment

start_of_32bit_protected_mode:

  .code32  # at this point the processor is in 32 bit protected mode

  movl  %esp,%ebp  # code generated by the "gcc" C compiler expects %ebp = %esp

  .globl __rtlib_entry

  jmp   __rtlib_entry  # jump to the C function "__rtlib_entry"

#------------------------------------------------------------------------------

# Interrupt handlers.

int0_intr:
  cli
  pushl $0x00
  pushl $0x00
  jmp int_handle_common_stub

int1_intr:
  cli
  pushl $0x00 # INT ARG
  pushl $0x01 # INT NO
  jmp int_handle_common_stub

int2_intr:
  cli
  pushl $0x00 # INT ARG
  pushl $0x02 # INT NO
  jmp int_handle_common_stub

int3_intr:
  cli
  pushl $0x00 # INT ARG
  pushl $0x03 # INT NO
  jmp int_handle_common_stub

int4_intr:
  cli
  pushl $0x00 # INT ARG
  pushl $0x04 # INT NO
  jmp int_handle_common_stub

int5_intr:
  cli
  pushl $0x00 # INT ARG
  pushl $0x05 # INT NO
  jmp int_handle_common_stub

int6_intr:
  cli
  pushl $0x00 # INT ARG
  pushl $0x06 # INT NO
  jmp int_handle_common_stub

int7_intr:
  cli
  pushl $0x00 # INT ARG
  pushl $0x07 # INT NO
  jmp int_handle_common_stub

int8_intr:
  cli
  # This interrupts pushes a real error code
  pushl $0x08 # INT NO
  jmp int_handle_common_stub

int9_intr:
  cli
  pushl $0x00 # INT ARG
  pushl $0x09 # INT NO
  jmp int_handle_common_stub

int10_intr:
  cli
  # This interrupts pushes a real error code
  pushl $0x0A # INT NO
  jmp int_handle_common_stub

int11_intr:
  cli
  # This interrupts pushes a real error code
  pushl $0x0B # INT NO
  jmp int_handle_common_stub

int12_intr:
  cli
  # This interrupts pushes a real error code
  pushl $0x0C # INT NO
  jmp int_handle_common_stub

int13_intr:
  cli
  # This interrupts pushes a real error code
  pushl $0x0D # INT NO
  jmp int_handle_common_stub

int14_intr:
  cli
  # This interrupts pushes a real error code
  pushl $0x0E # INT NO
  jmp int_handle_common_stub

int15_intr:
  cli
  pushl $0xA0 # INT ARG
  pushl $0x0F # INT NO
  jmp int_handle_common_stub

unhandled_intr:
  cli
  pushl $0xA0 # INT ARG
  pushl $99   # INT NO

int_handle_common_stub:
    pusha

    movw %ds, %ax
    pushl %eax

    cld

    .globl interrupt_handle
    call interrupt_handle

    popl %eax
    popa

    # Clean the stack
    addl $8, %esp

    sti
    iret

#------------------------------------------------------------------------------

# Trampolines into interrupt handlers written in C.

irq0_intr:

  .globl irq0

  cli
  pusha
  call  irq0
  popa
  sti
  iret

irq1_intr:

  .globl irq1

  cli
  pusha
  call  irq1
  popa
  sti
  iret

irq2_intr:

  .globl irq2

  cli
  pusha
  call  irq2
  popa
  sti
  iret

irq3_intr:

  .globl irq3

  cli
  pusha
  call  irq3
  popa
  sti
  iret

irq4_intr:

  .globl irq4

  cli
  pusha
  call  irq4
  popa
  sti
  iret

irq5_intr:

  .globl irq5

  cli
  pusha
  call  irq5
  popa
  sti
  iret

irq6_intr:

  .globl irq6

  cli
  pusha
  call  irq6
  popa
  sti
  iret

irq7_intr:

  .globl irq7

  cli
  pusha
  call  irq7
  popa
  sti
  iret

irq8_intr:

  .globl irq8

  cli
  pusha
  call  irq8
  popa
  sti
  iret

irq9_intr:

  .globl irq9

  cli
  pusha
  call  irq9
  popa
  sti
  iret

irq10_intr:

  .globl irq10

  cli
  pusha
  call  irq10
  popa
  sti
  iret

irq11_intr:

  .globl irq11

  cli
  pusha
  call  irq11
  popa
  sti
  iret

irq12_intr:

  .globl irq12

  cli
  pusha
  call  irq12
  popa
  sti
  iret

irq13_intr:

  .globl irq13

  cli
  pusha
  call  irq13
  popa
  sti
  iret

irq14_intr:

  .globl irq14

  cli
  pusha
  call  irq14
  popa
  sti
  iret

irq15_intr:

  .globl irq15

  cli
  pusha
  call  irq15
  popa
  iret

APIC_timer_intr:

  .globl APIC_timer_irq

  pushl %eax
  pushl %ebx
  pushl %ecx
  pushl %edx
  pushl %esi
  pushl %edi
  pushl %ebp
  call  APIC_timer_irq
  popl  %ebp
  popl  %edi
  popl  %esi
  popl  %edx
  popl  %ecx
  popl  %ebx
  popl  %eax
  iret

APIC_spurious_intr:

  .globl APIC_spurious_irq

  pushl %eax
  pushl %ebx
  pushl %ecx
  pushl %edx
  pushl %esi
  pushl %edi
  pushl %ebp
  call  APIC_spurious_irq
  popl  %ebp
  popl  %edi
  popl  %esi
  popl  %edx
  popl  %ecx
  popl  %ebx
  popl  %eax
  iret

sys_intr:

  .globl sys_irq

  cli
  pusha
  # make sure the C code does not affect ESP
  pushl %esp
  call  sys_irq
  popl  %esp
  popa
  sti

  iret
#------------------------------------------------------------------------------

# Video mode information.

  .align 2

  .globl video_mode

video_mode:
  .space 2

  .globl vbe_info

vbe_info:

VbeSignature:          .space 4
VbeVersion:            .space 2
OemStringPtr:          .space 4
Capabilities:          .space 4
VideoModePtr:          .space 4
TotalMemory:           .space 2
OemSoftwareRev:        .space 2
OemVendorNamePtr:      .space 4
OemProductNamePtr:     .space 4
OemProductRevPtr:      .space 4
                       .space 222
OemData:               .space 256

  .globl mode_info

mode_info:

ModeAttributes:        .space 2
WinAAttributes:        .space 1
WinBAttributes:        .space 1
WinGranularity:        .space 2
WinSize:               .space 2
WinASegment:           .space 2
WinBSegment:           .space 2
WinFuncPtr:            .space 4
BytesPerScanLine:      .space 2
XResolution:           .space 2
YResolution:           .space 2
XCharSize:             .space 1
YCharSize:             .space 1
NumberOfPlanes:        .space 1
BitsPerPixel:          .space 1
NumberOfBanks:         .space 1
MemoryModel:           .space 1
BankSize:              .space 1
NumberOfImagePages:    .space 1
                       .space 1
RedMaskSize:           .space 1
RedFieldPosition:      .space 1
GreenMaskSize:         .space 1
GreenFieldPosition:    .space 1
BlueMaskSize:          .space 1
BlueFieldPosition:     .space 1
RsvdMaskSize:          .space 1
RsvdFieldPosition:     .space 1
DirectColorModeInfo:   .space 1
PhysBasePtr:           .space 4
                       .space 4
                       .space 2
LinBytesPerScanLine:   .space 2
BnkNumberOfImagePages: .space 1
LinNumberOfImagePages: .space 1
LinRedMaskSize:        .space 1
LinRedFieldPosition:   .space 1
LinGreenMaskSize:      .space 1
LinGreenFieldPosition: .space 1
LinBlueFieldPosition:  .space 1
LinRsvdMaskSize:       .space 1
LinRsvdFieldPosition:  .space 1
MaxPixelClock:         .space 4
                       .space 191 # VBE 3.0 spec says: 189

  .globl modes

modes:
  .space 1024

#------------------------------------------------------------------------------

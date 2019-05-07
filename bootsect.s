# file: "bootsect.s"

# Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
# Rights Reserved.
#
# Revision History
# 22 Sep 01  initial version (Marc Feeley)

#------------------------------------------------------------------------------

KERNEL_SECTOR = 33              # sector number for "OS.SYS" file
INT13_READ_SECTOR_FN = 2        # BIOS int 0x13 function for "read sector"
INT10_TTY_OUTPUT_FN = 0xE       # BIOS int 0x10 function for "teletype output"
INT16_READ_KEYBOARD_FN = 0      # BIOS int 0x16 function for "read keyboard"
STACK_TOP = 0x10000             # location of stack top
SCRATCH = 0x1000                # location of scratch area

#------------------------------------------------------------------------------

  .globl bootsect_entry

bootsect_entry:

# Note: the BIOS always loads the boot sector of the disk at address 0000:7c00.

  .code16  # at this point the processor is in 16 bit real mode

#------------------------------------------------------------------------------

code_start:

# This header will make the boot sector look like the one for an MSDOS floppy.

  .byte 0xeb,0x3c,0x90 # this is a jump instruction to "after_header"
  .byte                0x2a,0x26,0x41,0x66,0x3c
  .byte 0x49,0x48,0x43
nb_bytes_per_sector:
  .byte                0x00,0x02
  .byte                          0x01,0x01,0x00
  .byte 0x02,0xe0,0x00,0x40,0x0b,0xf8,0x09,0x00
nb_sectors_per_track:
  .byte 0x12,0x00
nb_heads:
  .byte           0x02,0x00
  .byte                     0x00,0x00,0x00,0x00
  .byte 0x00,0x00,0x00,0x00
drive:
  .byte                     0x00
  .byte                          0x00,0x29,0xd1
  .byte 0x07,0x22,0x27,0x4f,0x53,0x20,0x20,0x20
  .byte 0x20,0x20,0x20,0x20,0x20,0x20,0x46,0x41
  .byte 0x54,0x31,0x32,0x20,0x20,0x20

#------------------------------------------------------------------------------

after_header:

# Setup segments.

  xorw  %cx,%cx
  movw  %cx,%ds
  movw  %cx,%ss
  movw  $(STACK_TOP & 0xffff),%sp

# Print banner.
  movw  $banner,%si
  call  print_string

# Enable A20 line so that odd and even megabytes can be accessed.

  call  test_a20     # check if A20 line is already enabled
  jz    a20_enabled

  movw  $0x64,%dx    # try to enable A20 line with the keyboard controller
  movb  $0xd1,%al
  outb  %al,%dx
  movb  $3,%al
  movw  $0x60,%dx
  outb  %al,%dx

  call  test_a20
  jz    a20_enabled

  movw  $0x92,%dx    # try to enable A20 line with the "fast A20 gate"
  inb   %dx,%al
  orb   $0x02,%al
  andb  $0xfe,%al
  outb  %al,%dx

  call  test_a20
  jz    a20_enabled

# Give up!

  movw  $a20_error,%si
  jmp   print_message_and_reboot

a20_enabled:

# Load kernel at "KERNEL_START".

  movl  $KERNEL_SECTOR,%eax
  movl  $KERNEL_START,%ebx
  movl  $KERNEL_SIZE,%ecx

  jmp   debug_string

next_sector:

  call  read_sector
  jnc   sector_was_read
  call  read_sector
  jnc   sector_was_read
  call  read_sector
  jc    cannot_load

sector_was_read:

  incl  %eax
  xorl  %edx,%edx
  movw  nb_bytes_per_sector,%dx
  addl  %edx,%ebx
  subl  %edx,%ecx

  ja    next_sector

# Turn off floppy disk's motor.

  movw  $0x3f2,%dx
  xorb  %al,%al
  outb  %al,%dx

# Jump to kernel.

  ljmp  $(KERNEL_START>>4),$0  # jump to "KERNEL_START" (which must be < 1MB)

cannot_load:

  movw  $load_error,%si

print_message_and_reboot:

  call  print_string

  movb  $INT16_READ_KEYBOARD_FN,%ah
  int   $0x16 # read keyboard

# Reboot.

  ljmp  $0xf000,$0xfff0  # jump to 0xffff0 (the CPU starts there when reset)

#------------------------------------------------------------------------------

test_a20:

# Test if the A20 line is disabled.  On return the Z flag is set if
# the A20 line is enabled and cleared if it is disabled.  We test it
# repeatedly because some hardware takes some time to enable the A20
# line.

  xorw  %cx,%cx 
test_a20_loop:
  movb  $0,SCRATCH
  movw  $0xffff,%ax
  movw  %ax,%es
  movb  %al,%es:SCRATCH+0x10
  testb %al,SCRATCH
  loopnz test_a20_loop
  ret

#------------------------------------------------------------------------------

read_sector:

# Read one sector from relative sector offset %eax (with bootsector =
# 0) to %ebx.

  pushl %eax
  pushl %ebx
  pushl %ecx

  pushl %eax
  pushl %ebx
  movw  $progress,%si
  call  print_string
  popl  %ebx
  popl  %eax

  movl  %eax,%edx
  shrl  $16,%edx
  divw  nb_sectors_per_track    # %ax = cylinder, %dx = sector in track
  incb  %dl
  movb  %dl,%cl
  xorw  %dx,%dx
  divw  nb_heads                # %ax = track, %dx = head
  shlb  $6,%ah
  orb   %ah,%cl
  movb  %al,%ch
  movb  %dl,%dh
  movb  drive,%dl
  movl  %ebx,%eax
  shrl  $4,%eax
  movw  %ax,%es
  andw  $0x0f,%bx
  movw  $(INT13_READ_SECTOR_FN<<8)+1,%ax
  int   $0x13

  popl  %ecx
  popl  %ebx
  popl  %eax
  ret

#------------------------------------------------------------------------------

# Print string utility.

print_string_loop:
  movb  $INT10_TTY_OUTPUT_FN,%ah
  movw  $(0<<8)+7,%bx   # page = 0, foreground color = 7
  int   $0x10
  incw  %si
print_string:
  movb  (%si),%al
  test  %al,%al
  jnz   print_string_loop
  ret

banner:
  .ascii "Loading"
  .byte 0

progress:
  .ascii "."
  .byte 0

a20_error:
  .byte 10,13
  .ascii "A20 line could not be enabled."

load_error:
  .byte 10,13
  .ascii "Could not load OS.  Press any key to reboot."
  .byte 10,13,0

debug_a:
.ascii "A"
.byte 0

debug_string:
  movw  $debug_a,%si
  call  print_string
  jmp debug_string


#------------------------------------------------------------------------------

code_end:

  .space (1<<9)-2-(code_end-code_start)  # skip to last 2 bytes of sector

  .byte 0x55  # valid boot sectors need these bytes at the end of the sector
  .byte 0xaa

#------------------------------------------------------------------------------

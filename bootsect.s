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

# ------------------------------------------------------------------------------

code_start:

# This header will make the boot sector look like the one for an MSDOS floppy.

  .byte 0xeb,0x3c,0x90 # this is a jump instruction to "after_header"
  .byte 0x2a,0x26,0x41,0x66,0x3c, 0x49,0x48,0x43 # OEM name, number
nb_bytes_per_sector:
  .byte 0x00,0x02 # bytes per sector (512 bytes)
  .byte 0x01      # sector per allocation unit -> sector/cluster
  .byte 0x01,0x00 # reserved sectors for booting (256)
  .byte 0x02      # number of FATs (always 2)
  .byte 0xe0,0x00 # number of root dir entries
  .byte 0x40,0x0b # number of logical sectors
  .byte 0xf0      # media descriptor byte (f0h: floppy, f8h: disk drive)
  .byte 0x09,0x00 # sectors per fat
nb_sectors_per_track:
  .byte 0x00,0x00 # sectors per track
nb_heads:
  .byte 0x00,0x00 # number of heads
  .byte 0x00,0x00 # number of hidden sectors
  .byte 0x00,0x00 # number of hidden sectors (high word)
  .byte 0x00,0x00,0x00,0x00 # total number of sectors in file system
drive:            # Extended block, supposed to be only for FAT 16
  .byte 0x00      # logical drive number
  .byte 0x00      # reserved
  .byte 0x29      # extended signature
  .byte 0xd1,0x07,0x22,0x27 # serial number
  .byte 0x4f,0x53,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20,0x20 # drive label
  .byte 0x46,0x41,0x54,0x31,0x32,0x20,0x20,0x20 # file system type (FAT 12)
# ------------------------------------------------------------------------------

after_header:

# Setup segments.
  cli

  xorw  %cx,%cx
  movw  %cx,%ds
  movw  %cx,%ss
  movw  $(STACK_TOP & 0xffff),%sp
  sti

# Print banner.
  movw  $banner,%si
  call  print_string

# Setup drive parameters, especially important for hard drive

  movb $0x08, %ah
  movb drive, %dl
  int $0x13

  jc cannot_load

  incb %dh                # dh is maximum head index, we increment by one to get head count
  movb %dh, nb_heads      # put the number of heads in the header
  andb $0x3f,%cl          # cl: 00s5......s1 (max sector)
  movb %cl, nb_sectors_per_track # the number of cylinder is useless for the LDA to CHS conversion

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

  call reset_drive

next_sector:

  call  read_sector
  jnc   sector_was_read
  call reset_drive

  call  read_sector
  jnc   sector_was_read

  call reset_drive
  call  read_sector

  # Failure: give up
  jc    cannot_load

# ------------------------------

sector_was_read:

  incl  %eax
  xorl  %edx,%edx
  movw  nb_bytes_per_sector,%dx
  addl  %edx,%ebx
  subl  %edx,%ecx

  ja    next_sector

# Turn off floppy disk's motor.

#  movw  $0x3f2,%dx
#  xorb  %al,%al
#  outb  %al,%dx

# Jump to kernel.

  movw $debug_a, %si
  call print_string

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

reset_drive:

  pushl %eax
  pushl %edx

  movb  drive, %dl
  xor %ax, %ax
  int $0x13

  popl %edx
  popl %eax

  ret  


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

  movl  %eax,%edx               # edx contains LDA
  shrl  $16,%edx                # dx contains LDA
  divw  nb_sectors_per_track    # %ax = cylinder, %dx = sector in track
  incw  %dx                     # increment sector
  movb  %dl,%cl                 # move the sector per track to cl
  xorw  %dx,%dx                 # erase dx
  divw  nb_heads                # %ax = track, %dx = head
  shlb  $6,%ah                  # keep the top 2 bits of track in ah
  orb   %ah,%cl                 # cl is top two bits of ah and sectors per track
  movb  %al,%ch                 # ch is the bottom part of the track
  movb  %dl,%dh                 # head is now in dh
  movb  drive,%dl               # dl is now the drive
  movl  %ebx,%eax               # put the target address in eax
  shrl  $4,%eax                 # div the address by 2^4
  movw  %ax,%es                 # insert the address in es
  andw  $0x0f,%bx
  movw  $0x0201,%ax # in AH, write 0x02 (command read) and in AL write 0x01 (1 sector to read)
  int   $0x13       # Call the read

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

  .space (1<<9)-(2 + 0)-(code_end-code_start)  # Skip to the end. The signature and the bootsector need to be written

  # Signature
  .byte 0x55
  .byte 0xaa
#------------------------------------------------------------------------------

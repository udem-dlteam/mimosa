# file: "bootsect.s"

# Copyright (c) 2019 by Marc Feeley and Universit� de Montr�al, All
# Rights Reserved.
#
# Revision History
# 22 Sep 01  initial version (Marc Feeley)

# ------------------------------------------------------------------------------
INT13_READ_SECTOR_FN = 2        # BIOS int 0x13 function for "read sector"
INT10_TTY_OUTPUT_FN = 0xE       # BIOS int 0x10 function for "teletype output"
INT16_READ_KEYBOARD_FN = 0      # BIOS int 0x16 function for "read keyboard"
STACK_TOP = 0x10000             # location of stack top
SCRATCH = 0x1000                # location of scratch area
ROOT_DIR_ENTRY_SIZE = 32        # the size for a root directory entry size
# ------------------------------------------------------------------------------

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
  .word 0x0200 # bytes per sector (512 bytes)
  .byte 0x01      # sector per allocation unit -> sector/cluster
nb_reserved_sectors:  
  .word 0x0001 # reserved sectors for booting 1
nb_of_fats:
  .byte 0x02      # number of FATs (usually 2)
nb_root_dir_entries:
  .word 0x00e8    # number of root dir entries
nb_logical_sectors:
  .word 0x0b40    # number of logical sectors
  .byte 0xF0      # media descriptor byte (f0h: floppy, f8h: disk drive)
nb_sectors_per_fat: 
  .word 0x0009    # sectors per fat
nb_sectors_per_track:
  .word 0x009 # sectors per track
nb_heads:
  .word 0x0000 # number of heads
nb_hidden_sectors:
  .long 0x00 # number of hidden sectors
  .byte 0x00,0x00,0x00,0x00 # total number of sectors in file system
drive:            # Extended block, supposed to be only for FAT 16
  .byte 0xAA      # logical drive number
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

  movb %dl, drive
  
# Get drive geometry
  movb $0x08, %ah
  int $0x13
  jc cannot_load

  incb %dh                # dh is maximum head index, we increment by one to get head count
  shrw $8, %dx            # place dh into dl (effictively, also keeps dh to 0x00)
  movw %dx, nb_heads      # put the number of heads in the header (max number + 1)
  andb $0x3f,%cl          # cl: 00s5......s1 (max sector)
  
  xorb %ch, %ch
  movw %cx, nb_sectors_per_track # the number of cylinder is useless for the LDA to CHS conversion

  # Prepare values for stage two loading

  movw $ROOT_DIR_ENTRY_SIZE, %ax # size in bytes of an entry in the root table
  xor %dx, %dx 
  mulw nb_root_dir_entries  # DX contains the high part, AX contains the low part of (number of entries * size of entries)
  # = directory size
  # Now we want the number of sectors occupied by the table
  divw nb_bytes_per_sector # ax now contains the number of sectors taken up by the table  
                           # dx contains the number of bytes extra
  movw %ax, nb_root_sectors

  # Cleanup
  xor %ax, %ax
  xor %dx, %dx
  
  # Calculating the start of the root dir
  movb nb_of_fats, %al
  mulw nb_sectors_per_fat
  shll $16, %edx             # set the high part
  movw %ax, %dx
  addw nb_hidden_sectors, %dx
  addw nb_reserved_sectors, %dx
  # edx now contains the sector of the root directory
  movl %edx, root_dir_sector # save it

  # We can now read the root directory to find the file we want
  # each sector read will contain 16 entries. Only the 11 first bytes are interesting to us
  # cx will contain the number of sectors read 

  xorl  %ecx, %ecx

  movl root_dir_sector, %eax

next_sector:

  # Are we done?
  cmp nb_root_sectors, %eax
  jc cannot_load
  
  movl  $SCRATCH, %ebx # write to scratch
  # Read sector edits es; save it
  call  read_sector
  # retry?
  jc    cannot_load
  # At scratch we now have a sector of the table

  check_entry:
    
    movw $11, %cx # The name is 11 bytes long
    movw $stage_2_name, %si # load the comparison pointer into si
    movw %bx, %di           # load the name pointer into di

    check_entry_loop:
    # compare the bytes at the pointer
    movb (%si), %dh
    movb (%di), %dl
    cmp  %dl, %dh
    
    jnz check_entry_no_match # the zero flag is set if there is equality

    # next character
    incw %si
    incw %di  

    decw %cx
    jz found_file           # if 0, jump to found file; we read all the characters without fault
    jmp check_entry_loop    # next character

    check_entry_no_match:

    addw $ROOT_DIR_ENTRY_SIZE, %bx   # move on to the next one
    cmp  nb_bytes_per_sector, %bx
    jc check_entry_done               # we read all the entries
    jmp check_entry # we did not read all the entries, continue

  check_entry_done:  
  incl %eax # we analysed a sector, go to the next one
  jmp next_sector

found_file:
  # At this point, bx contains the start of the root dir entry.
  # We want to read the cluster number, so we can look up the FAT and
  # finally read the file... The cluster is a word at offset 0x1A
  addw $0x1A, %bx
  xorw %ax, %ax
  movw %ax, %es
  movw %es:(%bx), %ax # cluster is now in ax
  pushw %ax # this is dirty; needs to be fixed

  # Fat offset:
  xorl %eax, %eax           # clean the upper part
  # movw nb_reserved_sectors, %ax
  # addw nb_hidden_sectors,   %ax # eax is now the reading address
  movw $1, %ax

  movl $SCRATCH, %ebx       # the destination address is now ebx

  movw nb_sectors_per_fat, %cx # numbers of sectors to read

  read_next_fat:

  call read_sector               # read a single sector
  incl %eax                      # get ready to read the next one
  addw nb_bytes_per_sector, %bx  # update the offset
  decw %cx                       # decrement cx. if cx is NOT zero, jump to read_next_fat
  jnz read_next_fat        

  # FAT loaded, looking for the file. We write it at KERNEL_START.
  xorl %eax, %eax                # make sure it's reset
  xorl %ecx, %ecx

  movl $KERNEL_START, %ebx       # we are going to write there

  popw %cx                       # cx now contains the cluster

  read_file_sector:

  movw %cx, %ax                  # sector to read is the cluster
  addw root_dir_sector, %ax      # next, add the offset in sectors of the root dir
  addw nb_root_sectors, %ax      # plus the length of the root dir
  subw $2,              %ax      # magic!

  # Read the first sector in the right memory spot
  call read_sector

  movw nb_bytes_per_sector, %ax
  addl %eax, %ebx # we filled the first 512 byte spot, move on

  # Now we need to lookup the FAT to figure out where is the next sector to read
  movl $SCRATCH, %edx          

  movw %cx, %si                  # get ready to read the fat entry
  shrw $1 , %si                  # get half of %si
  addw %cx, %si                  # si is now 1.5 time cx (the offset)

  addw %si, %dx                  # addr = base_addr + offset
  movw %dx, %si
  movw %ds:(%si), %dx            # read from mem into dx

  testw $0x0001, %cx                  # check if current cluster is odd
  jnz next_cluster_is_odd
  andw $0x0FFF, %dx
  jmp read_cluster_done
  
  next_cluster_is_odd:
  shrw $4, %dx

  read_cluster_done:
  movw %dx, %cx                  # set the current cluster
  cmpw $0xFF8, %cx               # if FAT is geq than the last block sym, we are done
  je start_kernel
  jl read_file_sector
  start_kernel:
  ljmp  $(KERNEL_START>>4),$0  # jump to "KERNEL_START" (which must be < 1MB)


read_sector:

# Read one sector from relative sector offset %eax (with bootsector =
# 0) to %ebx.
# CF = 1 if an error reading the sector occured
  pushw %es
  pushl %eax
  pushl %ebx
  pushl %ecx

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
  movw  $0x0201,%ax             # in AH, write 0x02 (command read) and in AL write 0x01 (1 sector to read)

  int   $0x13                   # Call the read
  jc cannot_load

  popl  %ecx
  popl  %ebx
  popl  %eax
  popw  %es

  ret

# ------------------------------------------------------------------------------
cannot_load:
  movw  $load_error,%si
  call print_string
  cli
  hlt

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

stage_2_name:
  .ascii "BOOT    SYS" # the extension is implicit, spaces mark blanks

load_error:
  .ascii "IO Error"
  .byte 0

nb_root_sectors:
  .long 0x00000 # number of sectors in the root directory
root_dir_sector:
  .long 0x00    # default value on floppy is 19; should be read correctly

code_end:
  .space (1<<9)-(2)-(code_end-code_start)  # Skip to the end (minus 2 for the signature)
# Signature
.byte 0x55
.byte 0xaa
#------------------------------------------------------------------------------
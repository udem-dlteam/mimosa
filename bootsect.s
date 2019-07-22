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
EXTENDED_MEM = 0x7E00           # location of the extended boot sector (0x7C00 + 512)
ROOT_DIR_ENTRY_SIZE = 32        # the size for a root directory entry size
ROOT_DIR_ENTRY_CLUSTER_HI_OFFSET = 0x14 # the root directory entry offset for the hi part of the cluster no
ROOT_DIR_ENTRY_CLUSTER_LO_OFFSET = 0x1A # the root directory entry offset for the lo part of the cluster no
# ------------------------------------------------------------------------------

.globl bootsect_entry

bootsect_entry:

# Note: the BIOS always loads the boot sector of the disk at address 0000:7c00.
.code16  # at this point the processor is in 16 bit real mode

# ------------------------------------------------------------------------------

code_start:

# This header will make the boot sector look like the one for an MSDOS floppy.

jmp after_header  # jump after the header block
.byte 0x00
oem_name:
  .ascii "MIMOSA"
  .byte 0
  .byte 0 # OEM name 8 characters (two spaces to get to 8)
nb_bytes_per_sector:
  .word 512    # bytes per sector (512 bytes)
nb_sectors_per_cluster:
  .byte 0x01      # sector per allocation unit -> sector/cluster
nb_reserved_sectors:  
  .word 32      # reserved sectors for booting (32 to get an extended boot sector (also FAT 32 compatible))
nb_of_fats:
  .byte 0x02      # number of FATs (usually 2)
nb_root_dir_entries:
  .word 0x00      # number of root dir entries (0 for FAT 32)
old_nb_logical_sectors:
  .word 0x00    # number of logical sectors on old devices
media_descriptor:
  .byte 0xF8      # media descriptor byte (f0h: floppy, f8h: disk drive)
old_nb_sectors_per_fat: 
  .word 0x00    # sectors per fat (0 For FAT32)
nb_sectors_per_track:
  .word 0x012     # sectors per track (cylinder)
nb_heads:
  .word 0x02 # number of heads
nb_hidden_sectors:
  .long 0x00 # number of hidden sectors
nb_logical_sectors:
  .long 65536
# --------------------------------------------------------------------------
# FAT 32 EBP
# --------------------------------------------------------------------------
nb_sectors_per_fat:
  .long 504
mirror_flags:
  .word 0x00                                                                   # TODO
fs_version:
  .word 0x00
first_cluster_root_dir:
  .long 0x02
fat_32_fs_region:
  .word 0x02      # We use 0x02 because 0x01 is the extended bootsector
backup_bootsec:
  .word 0xFFFF    # (none)
reserved_data:
  .long 0x00
  .long 0x00
  .long 0x00
drive:            # Extended block, supposed to be only for FAT 16
  .byte 0x80      # logical drive number
clean_fs:
  .byte 0x01      # reserved
extended_bpb_sig:
  .byte 0x29      # extended signature
serial_num:
  .byte 0xFF,0xFF,0xFF,0xFF # serial number
drive_lbl:
  .ascii "MIMOSA OS"
  .byte 0
  .byte 0 # drive label (11 char)
fs_label:
  .ascii "FAT32   " # file system type (FAT 12 here, 8 chars long)
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

  pushl %eax
  pushl %ebx

  movw $oem_name, %si
  call print_string

  movw $new_line, %si
  call print_string

  popl %ebx
  popl %eax
  
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
  
  # ------------------------------------------------------------------------------
  # Load the extended bootsector into the RAM
  # In order to use the extended bootsector correctly, the extended boot sector is 
  # loaded directly after the normal bootsector. This allows relative jumps in this
  # file to work correctly.

  movl $0x01, %eax # first sector of drive
  movl $EXTENDED_MEM, %ebx
  call read_sector

  # ------------------------------------------------------------------------------
  # Beyond this point, code in the extended bootsector can be correctly called
  # ------------------------------------------------------------------------------

  movw $extended_bootsector_loaded, %si
  call print_string

  # Prepare values for stage two loading

  movw $ROOT_DIR_ENTRY_SIZE, %ax # size in bytes of an entry in the root table
  xor %dx, %dx 
  mulw nb_root_dir_entries  # DX contains the high part, AX contains the low part of (number of entries * size of entries)
  # = directory size
  # Now we want the number of sectors occupied by the table
  divw nb_bytes_per_sector # ax now contains the number of sectors taken up by the table  
                           # dx contains the number of bytes extra
  movw %ax, nb_root_sectors

  # ------------------------------------------------------------------------------
  # Leave the common bootsector and jump to the extended bootsector
  # ------------------------------------------------------------------------------
  jmp extended_code_start


read_sector:

# Read one sector from relative sector offset %eax (with bootsector =
# 0) to %ebx.
# CF = 1 if an error reading the sector occured
# This function MUST be in the first sector of the bootsector(s)
  pusha

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

  popa

  ret

# ------------------------------------------------------------------------------
# Routines and functions
# ------------------------------------------------------------------------------

cannot_load:
  
  pushl %eax
  pushl %ebx

  movw $load_error_message, %si
  call print_string

failure_routine:
  movw $any_key_message, %si
  call print_string
  
  popl %ebx
  popl %eax

  xorb %ah, %ah          # Make sure it's on read key
  int $0x16
  
  ljmp  $0xf000,$0xfff0  # jump to 0xffff0 (the CPU starts there when reset)

nb_root_sectors:
  .long 0x00000 # number of sectors in the root directory
cluster_begin_lba:
  .long 0x00    # default value on floppy is 19; should be read correctly

any_key_reboot_msg:
  .ascii "\n\rPress any key to reboot"
  .byte 0

new_line:
  .ascii "\n\r"
  .byte 0

load_error_message:
  .ascii "IO Error. The system failed to load. Please reboot."
  .byte 0

any_key_message:
  .ascii "Press any key to reboot..."
  .byte 0

kernel_name:
  .ascii "BOOT    SYS" # the extension is implicit, spaces mark blanks

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
# ------------------------------------------------------------------------------
# MBR data
# ------------------------------------------------------------------------------
code_end:
  .space (1<<9)-(2 + 16 * 4)-(code_end-code_start)  # Skip to the end (minus 2 for the signature)
# Partition table (only one)

# partition 1
.byte 0x80                   # boot flag (0x00: inactive, 0x80: active)
.byte 0x00, 0x00, 0x00       # Start of partition address
.byte 0x0C                   # system flag (xFAT32, LBA access)
.byte 0x00, 0x00, 0x00       # End of partition address CHS : 79 1 18
.long 0x00                   # Start sector relative to disk
.long 65536                # number of sectors in partition

# partition 2
.byte 0x00                   # boot flag (0x00: inactive, 0x80: active)
.byte 0x00, 0x00, 0x00       # Start of partition address
.byte 0x00                   # system flag
.byte 0x00, 0x00, 0x00       # End of partition address
.long 0x00                   # Start sector relative to disk
.long 0x00 # number of sectors in partition

# partition 3
.byte 0x00                   # boot flag (0x00: inactive, 0x80: active)
.byte 0x00, 0x00, 0x00       # Start of partition address
.byte 0x00                   # system flag
.byte 0x00, 0x00, 0x00       # End of partition address
.long 0x00                   # Start sector relative to disk
.long 0x00 # number of sectors in partition

# partition 4
.byte 0x00                   # boot flag (0x00: inactive, 0x80: active)
.byte 0x00, 0x00, 0x00       # Start of partition address
.byte 0x00                   # system flag
.byte 0x00, 0x00, 0x00       # End of partition address
.long 0x00                   # Start sector relative to disk
.long 0x00 # number of sectors in partition

# Signature
.byte 0x55
.byte 0xAA

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------

extended_code_start:

  # Enable A20 line

  call  test_a20     # check if A20 line is already enabled
  jz    load_os

  movw  $0x64,%dx    # try to enable A20 line with the keyboard controller
  movb  $0xd1,%al
  outb  %al,%dx
  movb  $3,%al
  movw  $0x60,%dx
  outb  %al,%dx

  call  test_a20
  jz    load_os

  movw  $0x92,%dx    # try to enable A20 line with the "fast A20 gate"
  inb   %dx,%al
  orb   $0x02,%al
  andb  $0xfe,%al
  outb  %al,%dx

  call  test_a20
  jz    load_os
  jmp   a_20_failure

  test_a20:
  # Test if the A20 line is disabled.  On return the Z flag is set if
  # the A20 line is enabled and cleared if it is disabled.  We test it
  # repeatedly because some hardware takes some time to enable the A20

  xorw  %cx,%cx 
  test_a20_loop:
  movb  $0,SCRATCH
  movw  $0xffff,%ax
  movw  %ax,%es
  movb  %al,%es:SCRATCH+0x10
  testb %al,SCRATCH
  loopnz test_a20_loop
  ret

a_20_failure:

  movw $a_20_failure_message, %si
  call print_string
  
  jmp failure_routine  

load_os:
  xorw %ax, %ax
  movw %ax, %es # A20 messes with it

  # ----------------------------------------
  # Setup fat values
  # ----------------------------------------
  
  # Calculating the start of the root dir
  movb nb_of_fats, %al
  mull nb_sectors_per_fat
  shll $16, %edx             # set the high part (modified from the mul)
  movw %ax, %dx
  addw nb_hidden_sectors, %dx
  addw nb_reserved_sectors, %dx
  # edx now contains the sector of the root directory
  movl %edx, cluster_begin_lba # save it

  # We can now read the root directory to find the file we want
  # each sector read will contain 16 entries. Only the 11 first bytes are interesting to us
  # cx will contain the number of sectors read 

  # TODO: Find the cluster from the root directory
  movl first_cluster_root_dir, %eax # The base cluster is in the EPB

find_file:
  root_dir_read_loop:
  cmpl $0x0FFFFFF8, %eax  # compare the cluster with the end tag
  jge failure_routine # if cluster is geq end tag: done reading: no correct entry

  pushl %eax # save the cluster
  call cluster_to_lba # get the lba (eax is now LBA)
  
  xorl %ecx, %ecx
  movb nb_sectors_per_cluster, %cl # prepare the counter
  movl $SCRATCH, %ebx # We write the cluster to scratch
  root_dir_read_loop_sectors_in_cluster_loop:
    # eax is the LBA to read
    # ebx is the place to write the kernel

    call read_sector
    # Update the write pos
    pushl %eax  # protected the LBA
    xorl %eax, %eax
    movw nb_bytes_per_sector, %ax
    addl %eax, %ebx
    popl %eax  # eax contains the LBA again

    # Repeat until cluster is done
    incl %eax # move to the next block
    decb %cl  # one less block to read
    jnz root_dir_read_loop_sectors_in_cluster_loop
  # At this point, we need to search the scratch area for all the directory entries
  # A cluster is 8 sectors. A sectors is 512 byte, each entry is 32 bytes. 
  # Cleanup
  xorw %ax, %ax
  # Calculate how many entries we need to explore
  movb nb_sectors_per_cluster, %al
  mulw nb_bytes_per_sector # AX contains the current result
  movw $ROOT_DIR_ENTRY_SIZE, %cx
  divw %cx
  # AX nw contains the number of entries to scan
  movw %ax, %cx # Counter to when to stop
  movw $SCRATCH, %bx # load the address (a word is enough for the scratch area)
scan_next_dir_entry:
  pushw %cx # save the entry counter

  movw $11, %cx # The name is 11 bytes long
  movw $kernel_name, %si  # load the comparison pointer into si
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

  decw %cx                # One less character to read
  jnz check_entry_loop    # next character
                          # IF we did NOT jump, there is match
  # ----------------------------------------------------------
  # FILE MATCH
  # ----------------------------------------------------------  
  popw %cx # Dump that register from the stack (counter for entries)
  popl %eax # eax contains the current cluster (not important)
  xorl %eax, %eax

  pushl %ebx # Save the start for the low offset

  addw $ROOT_DIR_ENTRY_CLUSTER_HI_OFFSET, %bx
  movw %ax, %es # clear the segment
  movw %es:(%bx), %ax # hi cluster is now in ax
  shll $0x10, %eax # shift the hi part in place

  popl %ebx # Restore the start
  addw $ROOT_DIR_ENTRY_CLUSTER_LO_OFFSET, %bx
  movw %es:(%bx), %ax # lo cluster is now in ax

  jmp load_file # eax now contains the file start cluster
  # ----------------------------------------------------------
  # END OF FILE MATCH
  # ----------------------------------------------------------
  check_entry_no_match:

  popw %cx # Get the entry counter back
  decw %cx
  addw $ROOT_DIR_ENTRY_SIZE, %bx # Move on to the next entry
  jnz scan_next_dir_entry


  # Nothing in this cluster, move on to the next one
  popl %eax  # get the cluster
  call get_next_cluster # fetch the next cluster number
  jmp root_dir_read_loop

load_file:
  movl $KERNEL_START, %ebx # set the destination address
read_loop:
  cmpl $0x0FFFFFF8, %eax  # compare the cluster with the end tag
  jge start_kernel # if cluster is geq end tag: done reading

  pushl %eax # save the cluster
  call cluster_to_lba # get the lba (eax is now LBA)
  
  xorl %ecx, %ecx
  movb nb_sectors_per_cluster, %cl # prepare the counter
  sectors_in_cluster_loop:
    # eax is the LBA to read
    # ebx is the place to write the kernel

    call read_sector

    pushl %eax
    pushl %ebx
    movw $progress_dot, %si
    call print_string
    popl %ebx
    popl %eax

    # Update the write pos
    pushl %eax  # protected the LBA
    xorl %eax, %eax
    movw nb_bytes_per_sector, %ax
    addl %eax, %ebx
    popl %eax  # eax contains the LBA again

    # Repeat until cluster is done
    incl %eax # move to the next block
    decb %cl  # one less block to read
    jnz sectors_in_cluster_loop

  popl %eax  # get the cluster
  call get_next_cluster # fetch the next cluster number

  jmp read_loop

  start_kernel:
    cli
    ljmp  $(KERNEL_START>>4),$0  # jump to "KERNEL_START" (which must be < 1MB)

# ----------------------------------------------------------------------------------------------------------
# Functions
# ----------------------------------------------------------------------------------------------------------
cluster_to_lba:
  # Set the cluster into eax and eax will be set
  # to the LBA address that corresponds to the cluster
  pushl %edx

  xorl %edx, %edx

  subl $2, %eax
  movb nb_sectors_per_cluster, %dl # It kinda sucks that sectors per cluster is stored as an integer
  mull %edx                        # and not just a power of two. It would be faster and easier
  addl cluster_begin_lba, %eax
  
  popl %edx
  ret

get_next_cluster:
  
  pushl %ebx     # save registers that are affected
  pushl %edx

  andl $0x0FFFFFFFF, %eax # mask the four topmost bits
  xorl %ebx, %ebx
  xorl %edx, %edx
  # We want to get the next sector to read to get the correct FAT entry.
  # There are 512 bytes in a sector (usually). 4 bytes per entry. We divide
  # the nb of bytes per sector by 4 to get the correct division factor.
  movw nb_bytes_per_sector, %bx 
  shrw $2, %bx # div by 4
  divl %ebx   # eax contains the LBA to read (without the FAT offset)
  #             edx contains the offset in entries of 4 bytes of the next cluster

  # Load the FAT into the scratch area
  # FAT starts after reserved and hidden sectors, simply add those sectors
  addw nb_reserved_sectors, %ax
  addw nb_hidden_sectors,   %ax
  # EAX contains the correct offset

  movl $SCRATCH, %ebx
  call read_sector
  # Correct FAT is loaded into the FAT, edx is the offset

  movl (%ebx, %edx, 4), %eax # read from mem into eax.
  # At this point, eax will contain the next cluster

  popl %edx
  popl %ebx
  ret

# ----------------------------------------------------------------------------------------------------------
# Extended bootloader string and messages
# ----------------------------------------------------------------------------------------------------------
a_20_failure_message:
  .ascii "\n\rFailure to load the A20 line."
  .byte 0

progress_dot:
  .ascii "."
  .byte 0

extended_bootsector_loaded:
.ascii "\n\rBoot code correctly loaded."
.byte 0

extended_code_end:
.space (1<<9)-(2)-(extended_code_end-extended_code_start)  # Make that two sectors
# Signature of the second reserved block. This is not required (it's made up), but it allows debugging of the hex file format
.byte 0x55
.byte 0xBB
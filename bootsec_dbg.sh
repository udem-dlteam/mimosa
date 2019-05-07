#!/bin/bash
make build
mkdir debug
ssh administrator@localhost -p10022 "cat mimosa-build/bootsect.bin" > ./debug/bootsect.bin
qemu-system-i386 -s -S -hda ./debug/bootsect.bin

# Debug the disk identification process
# Maybe start like that?
#$ gdb
#(gdb) target remote localhost:1234
#(gdb) set architecture i386
#(gdb) break *0x7c00  # beginning of the bootsect, where the disk drive / floppy is detected
#(gdb) cont
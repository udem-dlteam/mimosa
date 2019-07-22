#!/bin/bash
mkdir -p /mnt/tmp

cp empty_usb.img mimosa-build/floppy.img
mount mimosa-build/floppy.img /mnt/tmp

cp mimosa-build/kernel.bin /mnt/tmp/BOOT.SYS

ls -al /mnt/tmp # List all the files in the img

umount /mnt/tmp
rm -rf /mnt/tmp

# Write the bootsector
hexdump -C -n 512 mimosa-build/floppy.img

dd if=mimosa-build/bootsect.bin of=mimosa-build/floppy.img bs=512 count=2 conv=notrunc

chmod 777 mimosa-build/floppy.img
#!/bin/bash
mkdir -p /mnt/tmp
mkfs.msdos -C mimosa-build/floppy.img 1440 -R 2 -v 
mount mimosa-build/floppy.img /mnt/tmp

cp mimosa-build/boot.bin /mnt/tmp/BOOT.SYS


umount /mnt/tmp
rm -rf /mnt/tmp

# Write the bootsector

hexdump -C -n 512 mimosa-build/floppy.img

dd if=mimosa-build/bootsect.bin of=mimosa-build/floppy.img bs=512 count=2 conv=notrunc

chmod 777 mimosa-build/floppy.img
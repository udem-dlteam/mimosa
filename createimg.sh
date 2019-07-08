#!/bin/bash
mkdir -p /mnt/tmp
cp blank_drive.img mimosa-build/floppy.img
mount mimosa-build/floppy.img /mnt/tmp

cp mimosa-build/boot.sys /mnt/tmp/BOOT.SYS


umount /mnt/tmp
rm -rf /mnt/tmp

# Write the bootsector

hexdump -C -n 512 mimosa-build/floppy.img


dd if=/dev/zero of=mimosa-build/tmp1.tmp bs=512 count=1
cat mimosa-build/tmp1.tmp mimosa-build/floppy.img > mimosa-build/floppy_2.img
cp mimosa-build/floppy_2.img mimosa-build/floppy.img
dd if=mimosa-build/bootsect.bin of=mimosa-build/floppy.img bs=512 count=2 conv=notrunc

chmod 777 mimosa-build/floppy.img
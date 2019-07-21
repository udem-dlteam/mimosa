#!/bin/bash
mkdir -p /mnt/tmp
mkfs.fat -C mimosa-build/floppy.img $((2 * 1024 * 1024)) -v -F 32
mount mimosa-build/floppy.img /mnt/tmp

objcopy -O binary mimosa-build/kernel.elf mimosa-build/boot.bin
cp test_programs/out/out.bin /mnt/tmp/OUT.BIN
cp /home/syvon/Desktop/TEST.TXT /mnt/tmp/TEST.TXT
cp mimosa-build/boot.bin /mnt/tmp/BOOT.SYS

ls -al /mnt/tmp
cat /mnt/tmp/TEST.TXT


umount /mnt/tmp
rm -rf /mnt/tmp

# Write the bootsector

hexdump -C -n 512 mimosa-build/floppy.img

dd if=mimosa-build/bootsect.bin of=mimosa-build/floppy.img bs=512 count=2 conv=notrunc

chmod 777 mimosa-build/floppy.img
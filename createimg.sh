#!/bin/bash
if [ ! -f ~/empty_usb.img ]; then
    echo "Empty drive not found!"
    echo "Decompressing the image..."
    tar -pxvzf ~/mimosa-build/empty_usb.tar.gz
    cp ~/mimosa-build/empty_usb.img ~/empty_usb.img
    rm ~/mimosa-build/empty_usb.img
fi

echo "Mount and write the OS unto the FS"
mkdir -p /mnt/tmp
cp ~/empty_usb.img ~/mimosa-build/floppy.img
echo "Mounting..."
mount -t vfat ~/mimosa-build/floppy.img /mnt/tmp -o loop

cp ~/mimosa-build/kernel.bin /mnt/tmp/BOOT.SYS
cp ~/mimosa-build/gsi.exe    /mnt/tmp/gsi.exe
# cp ~/mimosa-build/copypa.txt /mnt/tmp/copypa.txt

ls -al /mnt/tmp # List all the files in the img

umount /mnt/tmp
rm -rf /mnt/tmp

# Write the bootsector
hexdump -C -n 512 ~/mimosa-build/floppy.img
chmod 777 ~/mimosa-build/floppy.img
dd if=mimosa-build/bootsect.bin of=mimosa-build/floppy.img bs=512 count=2 conv=notrunc
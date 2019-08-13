#!/bin/bash
if [ ! -f ~/empty_usb.img ]; then
    echo "Empty drive not found!"
    echo "Decompressing the image..."
    tar -pxvzf ~/mimosa-build/empty_usb.tar.gz
    cp ~/mimosa-build/empty_usb.img ~/empty_usb.img
    rm ~/mimosa-build/empty_usb.img
fi

echo "Mount and write the OS onto the FS"
mkdir -p /mnt/tmp
cp ~/empty_usb.img ~/mimosa-build/floppy.img
echo "Mounting..."
mount -t vfat ~/mimosa-build/floppy.img /mnt/tmp -o loop

cp ~/mimosa-build/kernel.bin   /mnt/tmp/BOOT.SYS
cp -r ~/mimosa-build/archive-items/. /mnt/tmp/
cp -r ~/mimosa-build/archive-items/gambini /mnt/tmp/.gambini.scm
mkdir                                /mnt/tmp/sysfld

# mkdir /mnt/tmp/folder
# touch /mnt/tmp/folder/fif.tst
# echo "[0] This is a file pretty deep into the directory structure" >> /mnt/tmp/folder/fif.tst
# mkdir /mnt/tmp/folder/dfolder
# touch /mnt/tmp/folder/dfolder/dfif.tst
# echo "[1] This is a file pretty deep into the directory structure" >> /mnt/tmp/folder/dfolder/dfif.tst
# dd if=/dev/zero of=/mnt/tmp/dummy bs=512 count=2048

# cp ~/mimosa-build/copypa.txt /mnt/tmp/copypa.txt

ls -al /mnt/tmp # List all the files in the img

umount /mnt/tmp
rm -rf /mnt/tmp

# Write the bootsector
hexdump -C -n 512 ~/mimosa-build/floppy.img
chmod 777 ~/mimosa-build/floppy.img
dd if=mimosa-build/bootsect.bin of=mimosa-build/floppy.img bs=512 count=2 conv=notrunc

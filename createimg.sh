#!/bin/bash
if [ ! -f ./empty_usb.img ]; then
    echo "Empty drive not found!"
    echo "Decompressing the image..."
    tar -pxvzf ./empty_usb.tar.gz
    # cp ~/mimosa-build/empty_usb.img ~/empty_usb.img
    # rm ~/mimosa-build/empty_usb.img
fi

echo "Mount and write the OS onto the FS"
sudo mkdir -p /mnt/tmp
cp ./empty_usb.img ~/mimosa-build/floppy.img
echo "Mounting..."
sudo mount -t vfat ~/mimosa-build/floppy.img /mnt/tmp -o loop

cp kernel.bin /mnt/tmp/BOOT.SYS
cp -r ./archive-items/. /mnt/tmp/

# mkdir /mnt/tmp/folder
# touch /mnt/tmp/folder/fif.tst
# echo "[0] This is a file pretty deep into the directory structure" >> /mnt/tmp/folder/fif.tst
# mkdir /mnt/tmp/folder/dfolder
# touch /mnt/tmp/folder/dfolder/dfif.tst
# echo "[1] This is a file pretty deep into the directory structure" >> /mnt/tmp/folder/dfolder/dfif.tst
# dd if=/dev/zero of=/mnt/tmp/dummy bs=512 count=2048

# cp ~/mimosa-build/copypa.txt /mnt/tmp/copypa.txt

ls -al /mnt/tmp # List all the files in the img

sudo umount /mnt/tmp
sudo rm -rf /mnt/tmp

# Write the bootsector
hexdump -C -n 512 ./floppy.img
sudo chmod 777 ./floppy.img

sudo dd if=bootsect.bin of=floppy.img bs=512 count=2 conv=notrunc
echo "Image created"

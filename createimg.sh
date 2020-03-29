#!/bin/bash
set -e
if [ ! -f ./empty_usb.img ]; then
    echo "Empty drive not found!"
    echo "Decompressing the image..."
    tar -pxvzf ./empty_usb.tar.gz
    # cp ~/mimosa-build/empty_usb.img ~/empty_usb.img
    # rm ~/mimosa-build/empty_usb.img
fi

if [[ ! -f ./archive-items/gambit/bin ]]; then
  echo "Missing the Gambit archive... Downloading a default working version."
  mkdir ./archive-items/gambit/bin
  wget http://www-ens.iro.umontreal.ca/~yvonsamu/gsc.exe -O ./archive-items/gambit/bin/gsc.exe
fi

# Admin routines
admin-mount() {
  sudo mount $@
}

admin-umount() {
  : ${1:?"Expected path"}
  sudo umount "$1"
}

## Creating image
echo "Mount and write the OS onto the FS"
TMPDIR=$(mktemp -d)
cp ./empty_usb.img ./floppy.img

echo "Mounting..."
admin-mount -t vfat ./floppy.img "$TMPDIR" -o loop,uid=$UID
echo "Mounted"

echo "Copying items to the disk..."
cp kernel.bin "$TMPDIR/BOOT.SYS"
cp -r ./archive-items/. "$TMPDIR"

# mkdir /mnt/tmp/folder
# touch /mnt/tmp/folder/fif.tst
# echo "[0] This is a file pretty deep into the directory structure" >> /mnt/tmp/folder/fif.tst
# mkdir /mnt/tmp/folder/dfolder
# touch /mnt/tmp/folder/dfolder/dfif.tst
# echo "[1] This is a file pretty deep into the directory structure" >> /mnt/tmp/folder/dfolder/dfif.tst
# dd if=/dev/zero of=/mnt/tmp/dummy bs=512 count=2048

# cp ~/mimosa-build/copypa.txt /mnt/tmp/copypa.txt

ls -al "$TMPDIR" # List all the files in the img

admin-umount "$TMPDIR"
rmdir "$TMPDIR"

# Write the bootsector
hexdump -C -n 512 ./floppy.img

dd if=bootsect.bin of=floppy.img bs=512 count=2 conv=notrunc
echo "Image created"

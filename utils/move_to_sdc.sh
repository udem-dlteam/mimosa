#!/bin/bash
mkdir -p /mnt/tmp
sudo mount /dev/sdc /mnt/tmp

rm -rf /mnt/tmp/*
cp mimosa-build/boot.sys /mnt/tmp/BOOT.SYS

sudo umount /dev/sdc

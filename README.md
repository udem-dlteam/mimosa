# Mimosa
![Mimosa Logo](res/logo.png)<!-- .element height="50%" width="50%" -->

## Project scope

The mimosa repository contains the sources for a minimal operating
system for x86 (called Mimosa).  It is intended as a basis for benchmarking and
running Gambit. The idea is also to develop an operating system that uses the gambit environment
to execute userspace programs.

## Quick-install and run instructions

Assuming the ubuntu-6 ssh server is running (see
https://github.com/udem-dlteam/ubuntu-6), the following steps will
build and run mimosa:
    
    make clean
    make build
    make img # to create the disk image
    make run       # make debug is also available, it expects a debug connection to start


The ubuntu-6 system is required because it has gcc version 3.4 which
is *currently* required to build mimosa.  In the future this requirement
will be removed so that mimosa can build with recent versions of gcc.

The createimg.sh script is used to create a FAT32 image that can be mounted. Files can be added
to this image without a risk of making the system unbootable. 

To run mimosa with `make run` or `make debug`, you will need QEMU installed (at least `qemu-system-i386`)

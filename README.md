<h1 style="float:left">Mimosa</h1>
<img src="res/logo.png" width="80" >

<div style="clear:both"></div>

<img src="res/mimosa_screenshot.png" width="600">

## Project scope

The mimosa repository contains the sources for a minimal operating
system for x86 (called Mimosa).  It is intended as a basis for benchmarking and
running Gambit. 

Many objectives are targeted with the developement of Mimosa. While developping a benchmarking tool for 
generated Gambit code is of course a primary objective, the perspective of using Scheme to develop a complete
operating system is an intriguing one. This project can thefore be used as a jumping point towards a Scheme 
implementation and point of reference in terms of performance and structure. Many Scheme features are unknown
to traditional operating system development:

- Dynamic typing
- Automatic memory management
- Functional paradigm in general


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

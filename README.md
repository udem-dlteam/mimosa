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

Assuming you have the correct compiler setup, the following steps will
build and run mimosa:
    
    make clean
    make build
    make img # to create the disk image
    make run       # make debug is also available, it expects a debug connection to start

The createimg.sh script is used to create a FAT32 image that can be mounted. Files can be added
to this image without a risk of making the system unbootable. However, the folder `archive-items`will be replicated on the image.

To run mimosa with `make run` or `make debug`, you will need QEMU installed (at least `qemu-system-i386`)

## Required software

Currently, Mimosa builds with GCC 9.2.1. You will need the 32 bit tools in order to build the system.

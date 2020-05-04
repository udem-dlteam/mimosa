# file: "makefile"

OS_NAME = "\"MIMOSA version 2.0\""
KERNEL_START = 0x20000

KERNEL_OBJECTS = kernel.o libc/libc_os.o drivers/filesystem/vfs.o drivers/filesystem/stdstream.o main.o drivers/filesystem/fat.o drivers/ide.o disk.o thread.o chrono.o ps2.o term.o video.o intr.o rtlib.o uart.o heap.o bios.o $(NETWORK_OBJECTS)
#NETWORK_OBJECTS =
#NETWORK_OBJECTS = eepro100.o tulip.o timer2.o misc.o pci.o config.o net.o
DEFS = -DINCLUDE_EEPRO100

TARGET_ARCH=i386
GCC = gcc -m32 -Wno-write-strings -g -march=$(TARGET_ARCH)
GPP = g++ -m32 -Wno-write-strings -g -march=$(TARGET_ARCH)

SPECIAL_OPTIONS =

GCC_OPTIONS = $(SPECIAL_OPTIONS) $(DEFS) -DOS_NAME=$(OS_NAME) -DKERNEL_START=$(KERNEL_START) -fno-stack-protector -fomit-frame-pointer -fno-strict-aliasing -Wall -O3 -ffast-math -nostdinc -Iinclude -Ilibc -I/usr/include -ffreestanding -nostdlib

GPP_OPTIONS = $(GCC_OPTIONS) -fno-rtti -fno-builtin -fno-exceptions -nostdinc++

.SUFFIXES:
.SUFFIXES: .h .s .c .cpp .o .asm .bin .map .d

all: bin_files

single-archive: clean-bin
	$(MAKE)
	# cd .. ; sudo ./mimosa-build/createimg.sh;cd mimosa-build; tar czf mb.tar.gz kernel.bin kernel.elf bootsect.bin floppy.img;";
	./createimg.sh

build:
	mkdir -p mimosa-build
	tar --exclude='*.img' -czf - . mkdir -p mimosa-build;cd mimosa-build;tar xzf -;rm -rf kernel.bin; rm -rf bootsect.bin; rm -rf kernel.elf;make;tar czf mb.tar.gz kernel.bin kernel.elf bootsect.bin
	cp ./mimosa-build/mb.tar.gz ./
	tar xC mimosa-build -xzf mb.tar.gz

img:
	./mimosa-build/createimg.sh;tar czf flop.tar.gz mimosa-build/floppy.img
	cp administrator@localhost:~/flop.tar.gz ./
	tar xzf flop.tar.gz
	rm flop.tar.gz

run:
	qemu-system-i386 -s -m 1G -hda ./floppy.img -debugcon stdio

run-with-serial:
	qemu-system-i386 -s -m 1G -hda ./floppy.img -serial tcp:localhost:44555,server,nowait -serial pty -serial pty -debugcon stdio

debug:
	qemu-system-i386 -s -S -m 1G -hda ./floppy.img -debugcon stdio

mf:
	$(MAKE) clean
	$(MAKE) SPECIAL_OPTIONS=-MMD
	sed "/^# dependencies:$$/q" makefile | cat - *.d > mf
	rm -f *.d
	mv makefile makefile.old
	mv mf makefile

bin_files: bootsect.bin kernel.bin

kernel.bin: $(KERNEL_OBJECTS)
	ld -m elf_i386 --nostdlib --script=script.ld $(KERNEL_OBJECTS) -o $*.bin -Ttext $(KERNEL_START) --omagic --entry=kernel_entry --oformat elf32-i386 -Map kernel.map
	cp kernel.bin kernel.elf
	objcopy -O binary kernel.elf kernel.bin

kernel.bss:
	cat kernel.map | grep '\.bss ' | grep -v '\.o' | sed 's/.*0x/0x/'

kernel.o: kernel.s
ifeq "$(shell uname)" "Darwin"
	gcc -c -m32 -DKERNEL_START=$(KERNEL_START) -o $*.o $*.s;
else
	as --32 --defsym KERNEL_START=$(KERNEL_START) -o $*.o $*.s;
endif

.o.asm:
	objdump --disassemble-all $*.o > $*.asm

bootsect.o: bootsect.s kernel.bin
	as --32 --defsym KERNEL_START=$(KERNEL_START) --defsym KERNEL_SIZE=`cat kernel.bin | wc --bytes | sed -e "s/ //g"` -o $*.o $*.s

bootsect.bin: bootsect.o
	ld $*.o -o $*.bin -Ttext 0x7c00 --omagic --entry=bootsect_entry -m elf_i386 --oformat binary -Map bootsect.map

.cpp.o:
	$(GPP) $(GPP_OPTIONS) -c -o $*.o $*.cpp

.c.o:
	$(GCC) $(GCC_OPTIONS) -c -o $*.o $*.c

.s.o: kernel.bin
	as --32 --defsym OS_NAME=$(OS_NAME) --defsym KERNEL_START=$(KERNEL_START) --defsym KERNEL_SIZE=`cat kernel.bin | wc --bytes | sed -e "s/ //g"` -o $*.o $*.s

clean-archive-items:
	rm -f archive-items/home/sam/*

clean-bin:
	rm -f -- kernel.bin bootsect.bin kernel.elf

clean-libc:
	rm -f -- libc/libc_os.o

clean: clean-libc clean-archive-items
	rm -f -- *.o *.asm *.bin *.tmp *.d *.elf *.map floppy.img drivers/filesystem/stdstream.o drivers/filesystem/fat.o drivers/filesystem/vfs.o drivers/ide.o

# dependencies:
libc/libc_os.o: libc/libc_os.cpp \
                libc/include/dirent.h \
                libc/include/errno.h \
                libc/include/float.h \
                libc/include/libc_common.h \
                libc/include/libc_header.h \
                libc/include/libc_link.h \
                libc/include/libc_redirect.h \
                libc/include/limits.h \
                libc/include/math.h \
                libc/include/setjmp.h \
                libc/include/signal.h \
                libc/include/stddef.h \
                libc/include/stdio.h \
                libc/include/stdlib.h \
                libc/include/string.h \
                libc/include/sys/resource.h \
                libc/include/sys/time.h \
                libc/include/termios.h \
                libc/include/time.h \
                libc/include/unistd.h \
                libc/include/wchar.h \
                libc/src/dirent.c \
                libc/src/errno.c \
                libc/src/libc_link.c \
                libc/src/libc_support.c \
                libc/src/math.c \
                libc/src/setjmp.c \
                libc/src/signal.c \
                libc/src/stdio.c \
                libc/src/stdlib.c \
                libc/src/string.c \
                libc/src/sys_resource.c \
                libc/src/sys_time.c \
                libc/src/termios.c \
                libc/src/time.c \
                libc/src/unistd.c

# Dependencies generated by make-dependencies.py
heap.o: heap.cpp include/general.h include/heap.h include/rtlib.h include/term.h
ps2.o: ps2.cpp include/asm.h include/chrono.h drivers/filesystem/include/stdstream.h drivers/filesystem/include/vfs.h include/intr.h libc/include/libc_header.h include/ps2.h include/rtlib.h include/term.h include/thread.h include/video.h
chrono.o: chrono.cpp include/apic.h include/asm.h include/chrono.h include/intr.h include/rtc.h include/rtlib.h include/term.h include/thread.h
disk.o: disk.cpp include/disk.h include/ide.h include/rtlib.h include/term.h
rtlib.o: rtlib.cpp include/chrono.h include/disk.h drivers/filesystem/include/stdstream.h drivers/filesystem/include/vfs.h include/heap.h include/ide.h include/intr.h libc/include/libc_header.h include/ps2.h include/rtlib.h include/term.h include/thread.h include/video.h include/modifiedgambit.h
thread.o: thread.cpp include/apic.h include/asm.h include/chrono.h include/intr.h include/pic.h include/pit.h include/rtlib.h include/term.h include/thread.h include/general.h
main.o: main.cpp include/bios.h include/chrono.h include/disk.h drivers/filesystem/include/fat.h drivers/filesystem/include/stdstream.h drivers/filesystem/include/vfs.h include/general.h include/intr.h include/ps2.h include/rtlib.h include/term.h include/thread.h include/uart.h
video.o: video.cpp include/asm.h include/term.h include/vga.h include/video.h
term.o: term.cpp drivers/filesystem/include/stdstream.h drivers/filesystem/include/vfs.h include/ps2.h include/rtlib.h include/term.h include/thread.h
uart.o: uart.cpp include/asm.h include/general.h include/intr.h include/rtlib.h include/term.h include/thread.h include/uart.h
intr.o: intr.cpp include/apic.h include/asm.h include/intr.h include/pic.h include/rtlib.h include/term.h
bios.o: bios.cpp include/bios.h include/term.h
drivers/ide.o: drivers/ide.cpp include/ide.h include/asm.h include/disk.h include/intr.h include/rtlib.h include/term.h include/thread.h
drivers/filesystem/vfs.o: drivers/filesystem/vfs.cpp drivers/filesystem/include/vfs.h include/general.h drivers/filesystem/include/fat.h drivers/filesystem/include/stdstream.h include/rtlib.h include/term.h include/uart.h
drivers/filesystem/fat.o: drivers/filesystem/fat.cpp include/chrono.h include/disk.h include/general.h include/ide.h drivers/filesystem/include/fat.h drivers/filesystem/include/vfs.h include/rtlib.h include/thread.h
drivers/filesystem/stdstream.o: drivers/filesystem/stdstream.cpp drivers/filesystem/include/stdstream.h include/general.h drivers/filesystem/include/vfs.h include/rtlib.h include/thread.h


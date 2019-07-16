# file: "makefile"

OS_NAME = "\"MIMOSA version 1.2\""
KERNEL_START = 0x20000

KERNEL_OBJECTS = kernel.o main.o fs.o ide.o disk.o thread.o time.o ps2.o term.o video.o intr.o rtlib.o $(NETWORK_OBJECTS)
NETWORK_OBJECTS =
#NETWORK_OBJECTS = eepro100.o tulip.o timer2.o misc.o pci.o config.o net.o
DEFS = -DINCLUDE_EEPRO100 
#DEFS = -DINCLUDE_TULIP
#DEFS = -DINCLUDE_TULIP -DINCLUDE_EEPRO100 

GCC = gcc-3.4 -m32 -Wno-write-strings -g
GPP = g++-3.4 -m32 -Wno-write-strings -g

SPECIAL_OPTIONS =

GCC_OPTIONS = $(SPECIAL_OPTIONS) $(DEFS) -DOS_NAME=$(OS_NAME) -DKERNEL_START=$(KERNEL_START) -fomit-frame-pointer -fno-strict-aliasing -Wall -O3 -nostdinc -nostdinc++ -Iinclude

GPP_OPTIONS = $(GCC_OPTIONS) -fno-rtti -fno-builtin -fno-exceptions

.SUFFIXES:
.SUFFIXES: .h .s .c .cpp .o .asm .bin .map .d

all: floppy

build:
	mkdir -p mimosa-build
	tar cf - . | ssh administrator@localhost -p 10022 "rm -rf mimosa-build;mkdir mimosa-build;cd mimosa-build;tar xf -;make clean;make";ssh administrator@localhost -p 10022 "cat mimosa-build/floppy" > mimosa-build/floppy
	ssh administrator@localhost -p 10022 "cat mimosa-build/bootsect.bin" > mimosa-build/bootsect.bin
	ssh administrator@localhost -p 10022 "cat mimosa-build/kernel.bin"   > mimosa-build/boot.bin
	hexdump -C -n 512 mimosa-build/bootsect.bin

create-img:
	# Write the OS into the FS
	mkdir -p /mnt/tmp
	cp blank_drive.img mimosa-build/floppy.img
	mount mimosa-build/floppy.img /mnt/tmp

	cp mimosa-build/boot.bin /mnt/tmp/BOOT.SYS


	umount /mnt/tmp
	rm -rf /mnt/tmp

	# Write the bootsector
	dd if=mimosa-build/bootsect.bin of=mimosa-build/floppy.img conv=notrunc
	chmod 777 mimosa-build/floppy.img

run:
	qemu-system-x86_64 -s -S -m 4096 -hda mimosa-build/floppy.img -debugcon stdio

mf:
	make clean
	make SPECIAL_OPTIONS=-MMD
	sed "/^# dependencies:$$/q" makefile | cat - *.d > mf
	rm -f *.d
	mv makefile makefile.old
	mv mf makefile

floppy: bootsect.bin kernel.bin
	dd if=bootsect.bin of=tmp1.tmp bs=512 count=1
	dd if=blank_floppy of=tmp2.tmp bs=512 count=32 skip=1
	dd if=/dev/zero of=tmp3.tmp bs=512 count=2880
	cat tmp1.tmp tmp2.tmp kernel.bin tmp3.tmp > tmp4.tmp
	dd if=tmp4.tmp of=floppy bs=512 count=2880
	rm -f tmp1.tmp tmp2.tmp tmp3.tmp tmp4.tmp

kernel.bin: $(KERNEL_OBJECTS)
	ld --script=script.ld $(KERNEL_OBJECTS) -o $*.bin -Ttext $(KERNEL_START) --omagic --entry=kernel_entry --oformat binary -Map kernel.map

kernel.bss:
	cat kernel.map | grep '\.bss ' | grep -v '\.o' | sed 's/.*0x/0x/'

kernel.o: kernel.s
	as --defsym KERNEL_START=$(KERNEL_START) -o $*.o $*.s

.o.asm:
	objdump --disassemble-all $*.o > $*.asm

bootsect.o: bootsect.s kernel.bin
	as --defsym KERNEL_START=$(KERNEL_START) --defsym KERNEL_SIZE=`cat kernel.bin | wc --bytes | sed -e "s/ //g"` -o $*.o $*.s

bootsect.bin: bootsect.o
	ld $*.o -o $*.bin -Ttext 0x7c00 --omagic --entry=bootsect_entry --oformat binary -Map bootsect.map

.cpp.o:
	$(GPP) $(GPP_OPTIONS) -c $*.cpp

.c.o:
	$(GCC) $(GCC_OPTIONS) -c $*.c

.s.o: kernel.bin
	as --defsym OS_NAME=$(OS_NAME) --defsym KERNEL_START=$(KERNEL_START) --defsym KERNEL_SIZE=`cat kernel.bin | wc --bytes | sed -e "s/ //g"` -o $*.o $*.s

clean:
	rm -rf mimosa-build
	ssh administrator@localhost -p 10022 "rm -rf mimosa-build;"
	rm -f *.o *.asm *.bin *.tmp *.d

# dependencies:
config.o: config.c etherboot.h osdep.h include/asm.h include/general.h \
	nic.h pci.h cards.h
disk.o: disk.cpp include/disk.h include/general.h include/ide.h \
	include/thread.h include/intr.h include/asm.h include/pic.h \
	include/apic.h include/time.h include/pit.h include/queue.h \
	include/term.h include/video.h include/rtlib.h
eepro100.o: eepro100.c etherboot.h osdep.h include/asm.h \
	include/general.h nic.h pci.h cards.h timer2.h
# fifo.o: fifo.cpp include/fifo.h include/general.h include/thread.h \
	include/intr.h include/asm.h include/pic.h include/apic.h \
	include/time.h include/pit.h include/queue.h include/term.h \
	include/video.h include/rtlib.h
fs.o: fs.cpp include/fs.h include/general.h include/disk.h include/ide.h \
	include/thread.h include/intr.h include/asm.h include/pic.h \
	include/apic.h include/time.h include/pit.h include/queue.h \
	include/term.h include/video.h include/rtlib.h
ide.o: ide.cpp include/ide.h include/general.h include/thread.h \
	include/intr.h include/asm.h include/pic.h include/apic.h \
	include/time.h include/pit.h include/queue.h include/term.h \
	include/video.h include/rtlib.h include/disk.h
intr.o: intr.cpp include/intr.h include/general.h include/asm.h \
	include/pic.h include/apic.h include/term.h include/video.h
main.o: main.cpp include/general.h include/term.h include/video.h \
	include/thread.h include/intr.h include/asm.h \
	include/pic.h include/apic.h include/time.h include/pit.h \
	include/queue.h include/ps2.h
misc.o: misc.c etherboot.h osdep.h include/asm.h include/general.h
net.o: net.cpp include/net.h include/general.h include/rtlib.h \
	include/term.h include/video.h include/time.h include/asm.h \
	include/pit.h include/thread.h include/intr.h include/pic.h \
	include/apic.h include/queue.h etherboot.h osdep.h nic.h
pci.o: pci.c etherboot.h osdep.h include/asm.h include/general.h pci.h
ps2.o: ps2.cpp include/ps2.h include/general.h include/intr.h \
	include/asm.h include/pic.h include/apic.h include/time.h include/pit.h \
	include/video.h include/term.h include/thread.h include/queue.h
rtlib.o: rtlib.cpp include/rtlib.h include/general.h include/intr.h \
	include/asm.h include/pic.h include/apic.h include/time.h include/pit.h \
	include/ide.h include/thread.h include/queue.h include/term.h \
	include/video.h include/disk.h include/fs.h include/ps2.h
term.o: term.cpp include/term.h include/general.h include/video.h
thread.o: thread.cpp include/thread.h include/general.h include/intr.h \
	include/asm.h include/pic.h include/apic.h include/time.h include/pit.h \
	include/queue.h include/term.h include/video.h include/rtlib.h
time.o: time.cpp include/time.h include/general.h include/asm.h \
	include/pit.h include/apic.h include/intr.h include/pic.h include/rtc.h \
	include/term.h include/video.h
timer2.o: timer2.c etherboot.h osdep.h include/asm.h include/general.h \
	timer2.h
tulip.o: tulip.c etherboot.h osdep.h include/asm.h include/general.h \
	nic.h pci.h cards.h
video.o: video.cpp include/video.h include/general.h include/asm.h \
	include/vga.h include/term.h mono_5x7.cpp mono_6x9.cpp

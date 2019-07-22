GCC = gcc -m32 -Wno-write-strings -shared -ffreestanding

clean:
	rm -f func.o
	rm -f func.bin

build: func.bin


func.bin: func.o
	ld --script=script.ld  -nostartfiles --no-dynamic-linker -m elf_i386 -o func.bin--omagic --entry=main --oformat binary func.o

func.o:
	$(GCC) -c $*.c
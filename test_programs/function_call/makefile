GCC = gcc -m32 -Wno-write-strings

clean:
	rm -f func.o
	rm -f func.bin

build: func.bin


func.bin: func.o
	ld -m elf_i386 -o func.bin -Ttext 0x00 --omagic --entry=main --oformat binary func.o

func.o:
	$(GCC) -c $*.c
GCC = gcc -m32 -Wno-write-strings -shared -ffreestanding

clean:
	rm -f coucou.o
	rm -f coucou.bin

build: coucou.bin


coucou.bin: coucou.o
	ld -Ttext 0x100000 -m elf_i386 -o coucou.bin --omagic --entry=main --oformat binary coucou.o

coucou.o:
	$(GCC) -c $*.c
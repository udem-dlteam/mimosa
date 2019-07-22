GCC = gcc -m32 -Wno-write-strings

clean:
	rm -f out.o
	rm -f out.bin

build: out.bin


out.bin: out.o
	ld out.o -Ttext 0x00 --omagic --oformat binary out.bin

out.o:
	as -o out.o out.s
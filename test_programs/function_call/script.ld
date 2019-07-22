OUTPUT_FORMAT("binary")
OUTPUT_ARCH(i386)
ENTRY(main)
SECTIONS
{
  . = 0x50000;
  .text : { *(.text) }
  .data : { *(.data) }
  .bss  : { *(.bss) }
}

# file: "bootsect.s"

# Copyright (c) 2019 by Marc Feeley and Universit� de Montr�al, All
# Rights Reserved.
#
# Revision History
# 09 May 19  initial version (Samuel Yvon)
# Stage two of the bootloader.


STACK_TOP = 0x10000             # location of stack top
SCRATCH = 0x1000                # location of scratch area

.globl bootsect_stage_2


code_start:


code_end:
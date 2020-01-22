// file: "main.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Oct 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "chrono.h"
#include "disk.h"
#include "drivers/filesystem/include/stdstream.h"
#include "drivers/filesystem/include/vfs.h"
#include "drivers/filesystem/include/fat.h"
#include "general.h"
#include "include/intr.h"
#include "ps2.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"
#include "uart.h"
#include "bios.h"

int main() {

    debug_write("Main...");
    int pid = fork();
    debug_write("Back!");
    debug_write("Pid: ");
    debug_write(pid);

    while(1) {
        NOP();
        thread_yield();
    }



    return 0;
}

//-----------------------------------------------------------------------------

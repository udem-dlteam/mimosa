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

    int pid = fork();

    if(-1 == pid) {
        debug_write("Error while forking");
    } else if (0 == pid) {
        debug_write("Parent process");
    } else if(1 == pid) {
        debug_write("Child process");
    } else {
        debug_write("I don't know who I am.");
    }

    while(1) {
        thread_yield();
        /*
        if(1 == pid) {
            debug_write("C");
        } else {
            debug_write("P");
        }
        */
    }




    return 0;
}

//-----------------------------------------------------------------------------

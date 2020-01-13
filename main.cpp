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
#include "ps2.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"
#include "uart.h"
#include "bios.h"

int main() {
#ifdef BIOS_CALL_TEST
  {
    struct bios_call_regs r;
    bios_call(0x10, &r);
  }
#endif

#ifdef MIMOSA_REPL
  term_run(cout);
#endif

#ifdef GAMBIT_REPL
  {
    term_write(cout, "MIMOSA V.1.0.0\n");
    native_string file_name = "/dsk1/gambit/bin/gsi";
    // native_string file_name = "/dsk1/gsi";
    term_write(cout, "Starting ");
    term_write(cout, file_name);
    term_writeline(cout);

    file* prog = NULL;
    if (NO_ERROR == file_open(file_name, "r", &prog)) {
      uint32 len = file_len(prog);
      uint8* code = (uint8*)GAMBIT_START;
      term_write(cout, "The len of the gambit file is: ");
      term_write(cout, len);
      term_writeline(cout);

      error_code err;
      if (ERROR(err = file_read(prog, code, len))) {
        panic(L"Error while loading the file.");
      }

      term_write(cout, "Gambit file loaded...");
      term_writeline(cout);

      program_thread* task =
          CAST(program_thread*, kmalloc(sizeof(program_thread)));
      new_program_thread(task, "/dsk1/home/sam/", CAST(libc_startup_fn, code), "Gambit");
      thread_start(CAST(thread*, task));
#ifdef REMOTE_COM
        for(;;){
          file_read(stdout, &i, sizeof(unicode_char));
          native_char c = i & 0xFF;
          // send_serial(COM1_PORT_BASE, c);
          outb(c, COM1_PORT_BASE);
        }
#endif
    } else {
      term_write(cout, "\r\n Failed to open Gambit.\r\n");
    }
  }
#endif

#ifdef STREAM_STDOUT_TO_DEBUG_CONSOLE
  file* __stdout;
  if (ERROR(file_open(STDOUT_PATH, "rx", &__stdout))) {
    panic(L"Failed to open STDOUT");
  }

  unicode_char buff[512];
  error_code err;
#endif

  do {
#ifdef STREAM_STDOUT_TO_DEBUG_CONSOLE
    if (!ERROR(err = file_read(__stdout, buff, 512 * sizeof(unicode_char)))) {
      for (uint32 i = 0; i < (err / sizeof(unicode_char)); ++i) {
        _debug_write(CAST(native_char, buff[i] & 0xFF));
      }
    } else if (err != EOF_ERROR) {
      panic(L"Error!");
    }
#endif
    thread_yield();
  } while (1);

  return 0;
}

//-----------------------------------------------------------------------------

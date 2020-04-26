// file: "main.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Oct 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "bios.h" #include "chrono.h" #include "disk.h"
#include "drivers/filesystem/include/fat.h"
#include "drivers/filesystem/include/stdstream.h"
#include "drivers/filesystem/include/vfs.h"
#include "general.h"
#include "intr.h"
#include "ps2.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"
#include "uart.h"

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

    term_write(cout, "Enabling UART IRQ(s)");
    // Just in case?
    ENABLE_IRQ(3);
    ENABLE_IRQ(4);

    term_write(cout, "MIMOSA V.1.0.0\n");
    native_string file_name = "/dsk1/gambit/bin/gsc.exe";

    term_write(cout, "Starting ");
    term_write(cout, file_name);
    term_writeline(cout);

    file *prog = NULL;
    error_code fopen_error = NO_ERROR;
    if (NO_ERROR == (fopen_error = file_open(file_name, "r", &prog))) {
      uint32 len = file_len(prog);
      uint8 *code = (uint8 *)GAMBIT_START;
      term_write(cout, "The len of the gambit file is: ");
      term_write(cout, len);
      term_writeline(cout);

      error_code err;
      if (ERROR(err = file_read(prog, code, len))) {
        panic(L"Error while loading the file.");
      }

      term_write(cout, "Gambit file loaded...");
      term_writeline(cout);

      program_thread *task =
          CAST(program_thread *, kmalloc(sizeof(program_thread)));
      new_program_thread(task, "/dsk1/home/sam/", CAST(libc_startup_fn, code),
                         "Gambit");
      thread_start(CAST(thread *, task));
    } else {
      term_write(cout, "\r\n Failed to open Gambit.\r\n");
      term_write(cout, "Error code is ");
      term_write(cout, fopen_error);
      term_writeline(cout);
    }
  }
#endif

#ifdef STREAM_STDOUT_TO_DEBUG_CONSOLE
  file *__stdout;
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

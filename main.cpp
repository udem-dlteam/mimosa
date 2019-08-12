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

int main() {
#ifdef MIMOSA_REPL
  term_run(cout);
#endif

#ifdef GAMBIT_REPL
  {
    native_string file_name = "/dsk1/GSC.EXE";

    file* prog;
    if (NO_ERROR == file_open(file_name, "r", &prog)) {
      uint32 len = file_len(prog);
      uint8* code = (uint8*)GAMBIT_START;

      error_code err;
      if (ERROR(err = file_read(prog, code, len))) {
        panic(L"ERR");
      }

      program_thread* task =
          CAST(program_thread*, kmalloc(sizeof(program_thread)));
      new_program_thread(task, CAST(libc_startup_fn, code), "Gambit");
      thread_start(CAST(thread*, task));
    } else {
      term_write(cout, "\r\n Failed to open Gambit.\r\n");
    }
  }
#endif

  thread_sleep(seconds_to_time(30).n);

#ifdef STREAM_STDOUT_TO_DEBUG_CONSOLE
  file* __stdout;
  if (ERROR(file_open(STDOUT_PATH, "rx", &__stdout))) {
    panic(L"Nope");
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
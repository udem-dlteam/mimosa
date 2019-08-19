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

extern error_code read_lfn(fat_file* f, native_string* result);

int main() {
#ifdef SHOW_BOOT_TIME
  {
    uint8 hours, minutes, seconds;
    int16 year;
    uint8 month, day;
    
    get_current_time(&hours, &minutes, &seconds);
    get_current_date(&year, &month, &day);

    term_write(cout, "It is currently:\n");
    term_write(cout, hours);
    term_write(cout, " : ");
    term_write(cout, minutes);
    term_write(cout, " : ");
    term_write(cout, seconds);
    term_writeline(cout);
    term_write(cout, "On the: \n");
    term_write(cout, day);
    term_write(cout, " : ");
    term_write(cout, month);
    term_write(cout, " : ");
    term_write(cout, year);
    term_writeline(cout);
  }
#endif
  error_code err = NO_ERROR;
  native_string to_send = "this is a big string that must be sent through the COM PORT";
  file* com_port_one;

  if(ERROR(err = file_open(COM1_PATH, "r", &com_port_one))) {
    term_write(cout, err);
    panic(L"Failed to open an handle on COM1");
  }

  native_string p = to_send;
  uint32 len = kstrlen(to_send);
  debug_write("Sendign..."); debug_write(to_send); debug_write("chars");
  if(ERROR(file_write(com_port_one, to_send, kstrlen(to_send)))) {
    term_write(cout, err);
    panic(L"Failed to write to COM 1");
  }

  native_char c;
  while(1) {
    if(ERROR(err = file_read(com_port_one, &c, sizeof(native_char)))) {
      debug_write("--READING");
      _debug_write(c);
      term_write(cout, err);
      panic(L"Failed to read COM 1");
    } else if(err > 0) {
      debug_write("--READING");
      _debug_write(c);
    }
  }  

#ifdef MIMOSA_REPL
  term_run(cout);
#endif

#ifdef GAMBIT_REPL
  {
    native_string file_name = "/dsk1/GSI.EXE";

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
      new_program_thread(task, "/dsk1", CAST(libc_startup_fn, code), "Gambit");
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

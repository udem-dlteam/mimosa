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

#ifdef MIMOSA_REPL
  error_code err;
  file* f;
  if(ERROR(err = file_open("/dsk1/longfilenametest.scm", "a+", &f))) {
    panic(L"Failed to open the file");
  }

  term_write(cout, "The file name is: ");
  term_write(cout, f->name);
  
  term_write(cout, "Creating a folder....");

  if(ERROR(err = mkdir("/dsk1/abigfolder", &f))) {
    panic(L"This is an error");
  }

  if(ERROR(err = file_open("/dsk1/abigfolder/abigsubfileintoafolder", "a+", &f))) {
    panic(L"Another error");
  }

  if(ERROR(err = file_rename("/dsk1/abigfolder/abigsubfileintoafolder", "/dsk1/abigfilerenamedacrossafolder"))) {
    term_write(cout, "Error is: ");
    term_write(cout, err);
    panic(L"Failed to rename");
  }
  
  if(ERROR(err = file_remove("dsk1/abigfilerenamedacrossafolder"))) {
    term_write(cout, "Error is");
    term_write(cout, err);
    panic(L"Failed to remove");
  }

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
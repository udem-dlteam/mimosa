// file: "main.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Oct 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "chrono.h"
#include "disk.h"
#include "fat32.h"
#include "fs.h"
#include "general.h"
#include "ps2.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"

rwmutex* m;
volatile uint32 print;

void readroutine() {
  for(;;) {
    __surround_with_debug_t("Read lock", rwmutex_readlock(m););
    debug_write(print);    
    __surround_with_debug_t("Read unlock", rwmutex_readunlock(m););
  }
}

void writeroutine() {
  for (;;) {
    __surround_with_debug_t("Write lock", rwmutex_writelock(m););
    print++;
    __surround_with_debug_t("Write unlock", rwmutex_writeunlock(m););
  }
}

int main() {
  term tty;

  term_init(&tty, 0, 366, 80, 13, &font_mono_5x7, &font_mono_5x7, L"tty", TRUE);

#ifdef MIMOSA_REPL
  term_run(&tty);
#endif

#ifdef GAMBIT_REPL
  {
    native_string file_name = "GSC.EXE";

    file* prog;
    if (NO_ERROR == open_file(file_name, "r", &prog)) {
      term_write(&tty, "\r\n Starting program ");
      term_write(&tty, file_name);
      term_writeline(&tty);

      // TODO:
      // The program thread needs to be aware of what its doing
      uint32 len = prog->length;
      uint8* code = (uint8*)GAMBIT_START;

      error_code err;
      if (ERROR(err = read_file(prog, code, len))) {
        panic(L"ERR");
      }

      term_write(cout, "File loaded. Starting program at: ");
      term_write(cout, code);

      thread_sleep(1000);

      for (int i = 0; i < 5; ++i) {
        term_writeline(cout);
      }

      program_thread* task =
          CAST(program_thread*, kmalloc(sizeof(program_thread)));
      new_program_thread(task, CAST(libc_startup_fn, code), "Gambit");
      thread_start(CAST(thread*, task));

    } else {
      term_write(&tty, "\r\n Failed to open the program.\r\n");
    }
  }
#else 
  // Test
  m = CAST(rwmutex*, kmalloc(sizeof(rwmutex)));

  thread* read1 = CAST(thread*, kmalloc(sizeof(thread)));
  thread* read2 = CAST(thread*, kmalloc(sizeof(thread)));
  thread* wrrt1 = CAST(thread*, kmalloc(sizeof(thread)));

  // sched_stats();

  thread_start(new_thread(read1, readroutine, "READ1"));
  thread_start(new_thread(read2, readroutine, "READ2"));
  thread_start(new_thread(wrrt1, writeroutine, "WRRT1"));

  // sched_stats();

#endif

  // Never exit, but never do anything either
  for (;;) {
    // sched_stats();
    disable_interrupts();

    term_write(cout, "[1] Sanity:");
    term_write(cout, m->_readers);
    term_writeline(cout);
    term_write(cout, "[2] Sanity:");
    term_write(cout, m->_writerq);
    term_writeline(cout);
    enable_interrupts();

    thread_sleep(seconds_to_time(10).n);
  }

  return 0;
}

//-----------------------------------------------------------------------------
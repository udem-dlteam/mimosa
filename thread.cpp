// file: "thread.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Oct 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "thread.h"
#include "asm.h"
#include "pic.h"
#include "apic.h"
#include "pit.h"
#include "intr.h"
#include "chrono.h"
#include "rtlib.h"
#include "term.h"

wait_queue* readyq;
sleep_queue* sleepq;
thread* sched_primordial_thread;
thread* sched_current_thread;

//-----------------------------------------------------------------------------

// // "primordial_thread" class.

// class primordial_thread : public thread
//   {
//   public:

//     primordial_thread (void_fn continuation);
//     virtual void run ();
//     virtual char* name ();

//   private:

//     void_fn _continuation;
//   };

//   char* primordial_thread_name() { return "PRIMORDIAL_THREAD"; }

//   primordial_thread_primordial_thread(void_fn continuation) {
//     _continuation = continuation;
// }

// void primordial_thread_run ()
// {
//   _continuation ();
// }

//-----------------------------------------------------------------------------
// C Rewrite
//-----------------------------------------------------------------------------

mutex* new_mutex(mutex* m) {
  wait_queue_init(&m->super);
  m->_locked = false;
  sched_reg_mutex(m);
  return m;
}

rwmutex* new_rwmutex(rwmutex* rwm) {
  new_mutex(CAST(mutex*, rwm));
  rwm->_readers = 0;
  rwm->_writerq = 0;
  return rwm;
}

void mutex_lock(mutex* self) {
  disable_interrupts();

  if (self->_locked) {
    save_context(_sched_suspend_on_wait_queue, &self->super);
  } else {
    self->_locked = TRUE;
  }
  enable_interrupts();
}

void rwmutex_readlock(rwmutex* self) {
  disable_interrupts();

  mutex* mself = &self->super;

  while (mself->_locked || self->_writerq > 0) {
    save_context(_sched_suspend_on_wait_queue, &mself->super);
  }

  self->_readers++;

  enable_interrupts();
}

void rwmutex_writelock(rwmutex* self) {
  bool was_waiting;
  disable_interrupts();

  mutex* mself = &self->super;

  if(was_waiting = (mself->_locked || self->_readers > 0)) self->_writerq++;

  while (mself->_locked || self->_readers > 0) {
    save_context(_sched_suspend_on_wait_queue, &mself->super);
  }

  if(was_waiting) self->_writerq--;
  mself->_locked = TRUE;

  enable_interrupts();
}

void rwmutex_readunlock(rwmutex* self) {
  thread* t;
  mutex* mself = &self->super;
  disable_interrupts();

  self->_readers--;
  while ((t = wait_queue_head(&mself->super)) != NULL) {
    sleep_queue_remove(t);
    sleep_queue_detach(t);
    _sched_reschedule_thread(t);
  }
  _sched_yield_if_necessary();

  enable_interrupts();
}

void rwmutex_writeunlock(rwmutex* self) {
  thread* t;
  mutex* mself = &self->super;
  disable_interrupts();

  self->super._locked = FALSE;
  while ((t = wait_queue_head(&mself->super)) != NULL) {
    sleep_queue_remove(t);
    sleep_queue_detach(t);
    _sched_reschedule_thread(t);
  }
  _sched_yield_if_necessary();

  enable_interrupts();
}

void mutex_unlock(mutex* self) {
  disable_interrupts();

  thread* t = wait_queue_head(&self->super);

  if (t == NULL) {
    self->_locked = FALSE;
  } else {
    sleep_queue_remove(t);
    sleep_queue_detach(t);
    _sched_reschedule_thread(t);
    _sched_yield_if_necessary();
  }

  enable_interrupts();
}

bool mutex_lock_or_timeout(mutex* self, time timeout) {
    disable_interrupts();

  thread* current = sched_current_thread;

  if (self->_locked) {

    if ((timeout.n == 0) | !less_time(current_time_no_interlock(), timeout)) {
      enable_interrupts();
      return FALSE;
    }

    current->_timeout = timeout;
    current->_did_not_timeout = TRUE;

    // Remove it from whatever it's waiting
    wait_queue_remove(current);
    wait_queue_detach(current);
    // Wait on the mutex: we are blocked until the mutex is freed
    wait_queue_insert(current, &self->super);
    // Sleep
    save_context(_sched_suspend_on_sleep_queue, NULL);
    // 
    enable_interrupts();

    return self->_locked = current->_did_not_timeout;
  }

  self->_locked = TRUE;

  enable_interrupts();

  return TRUE;
}

mutex* seq;////////////////////

condvar* new_condvar(condvar* c) {
  wait_queue_init(&c->super);
  sched_reg_condvar(c);
  return c;
}


void condvar_wait(condvar* self, mutex* m) {
  disable_interrupts();

  thread* t = wait_queue_head(&m->super);

  if (t == NULL) {
    m->_locked = FALSE;
  } else {
    sleep_queue_remove(t);
    sleep_queue_detach(t);
    _sched_reschedule_thread(t);
  }

  if (m->_locked) {
    save_context(_sched_suspend_on_wait_queue, m);
  } else {
    m->_locked = TRUE;
  }

  enable_interrupts();
}

bool condvar_wait_or_timeout(condvar* self, mutex* m, time timeout) {
  disable_interrupts();

  thread* current = sched_current_thread;

  thread* t = wait_queue_head(CAST(wait_queue*, m));

  if (t == NULL) {
    m->_locked = FALSE;
  } else {
    sleep_queue_remove(t);
    sleep_queue_detach(t);
    _sched_reschedule_thread(t);
  }

  if (!less_time(current_time_no_interlock(), timeout)) {
    enable_interrupts();
    return FALSE;
  }

  current->_timeout = timeout;
  current->_did_not_timeout = TRUE;

  wait_queue_remove(current);
  wait_queue_insert(current, &self->super);

  save_context(_sched_suspend_on_sleep_queue, NULL);

  ASSERT_INTERRUPTS_DISABLED();

  if (current->_did_not_timeout) {
    enable_interrupts();
    return mutex_lock_or_timeout(m, timeout);
  }

  enable_interrupts();

  return FALSE;
}

void condvar_signal(condvar* self) {
  disable_interrupts();

  thread* t = wait_queue_head(&self->super);

  if (t != NULL) {
    sleep_queue_remove(t);
    sleep_queue_detach(t);
    _sched_reschedule_thread(t);
    _sched_yield_if_necessary();
  }

  enable_interrupts();
}

void condvar_broadcast(condvar* self) {
  disable_interrupts();

  thread* t;

  while ((t = wait_queue_head(&self->super)) != NULL) {
    sleep_queue_remove(t);
    sleep_queue_detach(t);
    _sched_reschedule_thread(t);
  }

  _sched_yield_if_necessary();
  enable_interrupts();
}

void condvar_mutexless_wait(condvar* self) {
  // Interrupts should be disabled at this point
  ASSERT_INTERRUPTS_DISABLED();  
  
  save_context(_sched_suspend_on_wait_queue, &self->super);
  
  ASSERT_INTERRUPTS_DISABLED();
}

void condvar_mutexless_signal(condvar* self) {
  ASSERT_INTERRUPTS_DISABLED();  // Interrupts should be disabled at this point

  thread* t = wait_queue_head(&self->super);

  if (t != NULL) {
    sleep_queue_remove(t);
    sleep_queue_detach(t);
    _sched_reschedule_thread(t);
    _sched_yield_if_necessary();
  }

  ASSERT_INTERRUPTS_DISABLED();
}

// "thread" class implementation.

program_thread* new_program_thread(program_thread* self, native_string cwd,
                                   libc_startup_fn run, native_string name) {
  new_thread(&self->super, NULL, name);
  self->super.type = THREAD_TYPE_USER;
  self->super._prio = high_priority;
  self->_code = run;
  self->_cwd = NULL;
  self->super.vtable = &_program_thread_vtable;

  uint32 len = kstrlen(cwd)+1;
  self->_cwd = CAST(native_string, kmalloc(sizeof(native_char) * len));
  memcpy(self->_cwd, cwd, len);

  return self;
}

native_string program_thread_cwd(program_thread* self) { return self->_cwd; }

native_string program_thread_chdir(program_thread* self,
                                   native_string new_cwd) {
  native_string old = self->_cwd;
  uint32 len = kstrlen(new_cwd)+1;

  self->_cwd = CAST(native_string, kmalloc(sizeof(native_char) * (len + 1)));
  memcpy(self->_cwd, new_cwd, len + 1);
  self->_cwd = new_cwd;
  if(NULL != old) kfree(old);

  return self->_cwd;
}

thread* new_thread (thread* self, void_fn run, native_string name)
{
  static const int stack_size = 65536 << 1; // size of thread stacks in bytes

  mutex_queue_init(&self->super.super);
  wait_queue_detach(self);
  sleep_queue_detach(self);

  uint32* s = CAST(uint32*, kmalloc(stack_size));

  if (s == NULL)
    panic(L"out of memory");

  self->_stack = s;

  s += stack_size / sizeof (uint32);


  // Stack frame we want to build:
  //  ---------------------------
  // |        GEN-PURPOSE        |
  //  ---------------------------
  // |           EFLAGS          |
  //  ---------------------------
  // |             |       CS    |
  //  ---------------------------
  // |            EIP            |
  //  ---------------------------  
  // Room for pushall, all initiated at zero
  // This stack frame is built in order to be compatible with the 
  // context switching routines that expected the general purpose registers
  // to be before the IRET frame.
  for (int i = 0; i < 8; i++) {
    *-- s = 0;
  }

  *--s = 0;              // the (dummy) return address of "run_thread"
  *--s = (eflags_reg() | (1 << 9));  // space for "EFLAGS"
  // We XOR the interrupt activated so the first thread start will
  // have interrupts on regardless of the interrupt status
  // of the creation site.
  *--s = cs_reg ();      // space for "%cs"
  *--s = CAST(uint32,&_sched_run_thread); // to call "run_thread"


  // Note: the 3 bottommost words on the thread's stack are in the same
  // layout as expected by the "iret" instruction.  When an "iret"
  // instruction is executed to restore the thread's context, the
  // function "run_thread" will be called and this function will get a
  // dummy return address (it is important that the function
  // "run_thread" never returns). The general purpose is used for 
  // correct context switching and restoring between tasks.
  self->_sp = s;
  self->_quantum = frequency_to_time(200);  // quantum is 1/200th of a second
  self->_prio = normal_priority;
  self->_terminated = FALSE;
  self->_run = run;
  self->_name = name;
  self->type = THREAD_TYPE_KERNEL;

  self->vtable = &_thread_vtable;

  return self;
}

thread* thread_start(thread* self) {
  disable_interrupts();

  {
    _sched_reschedule_thread(self);
    _sched_yield_if_necessary();
  }

  enable_interrupts();

  return self;
}

void thread_join (thread* self)
{
  mutex_lock(&self->_m);
  while (!self->_terminated)
    condvar_wait(&self->_joiners, &self->_m);
  mutex_unlock(&self->_m);
}

void thread_yield() {

  disable_interrupts();

  save_context(_sched_switch_to_next_thread, NULL);

  enable_interrupts();
}

thread* thread_self() { return sched_current_thread; }

void thread_sleep_seconds(uint64 seconds) {
  disable_interrupts();

  {
    thread* current = sched_current_thread;

    current->_timeout =
        add_time(current_time_no_interlock(), seconds_to_time(seconds));

    wait_queue_remove(current);
    wait_queue_detach(current);

    save_context(_sched_suspend_on_sleep_queue, NULL);
  }

  enable_interrupts();
}

void thread_sleep(uint64 timeout_nsecs) {
#ifdef BUSY_WAIT_INSTEAD_OF_SLEEP
  for (int i = 0; i < 1; ++i) {
    for (int j = 0; j < timeout_nsecs; ++j) {
      __asm__ __volatile__ ("NOP" : : : "memory");
    }
  }
#else
  disable_interrupts();

  {
    thread* current = sched_current_thread;

    current->_timeout = add_time(current_time_no_interlock(),
                                 nanoseconds_to_time(timeout_nsecs));

    wait_queue_remove(current);
    wait_queue_detach(current);

    save_context(_sched_suspend_on_sleep_queue, NULL);
  }

  enable_interrupts();
#endif
}

#define thread_run(t) do {t->vtable->thread_run(t);} while(0)

void virtual_thread_run(thread* self) { self->_run(); }

void virtual_program_thread_run(thread* sself) {
  program_thread* self = CAST(program_thread*, sself);
  term_write(cout, "Running program thread");
  term_writeline(cout);
  static char* argv[] = {"app", "-:t4,:~~lib,:~~userlib", NULL};
  int argc = sizeof(argv) / sizeof(argv[0]) - 1;
  static char* env[] = {NULL};
  self->_code(argc, argv, env);
}

native_string thread_name(thread* self) { return self->_name; }


//-----------------------------------------------------------------------------

// "scheduler" class implementation.

mutex* mtab[100]; int mn = 0;
condvar* ctab[100]; int cn = 0;

void sys_irq (void* esp)///////////////// AMD... why do we need this hack???
{
  ASSERT_INTERRUPTS_DISABLED ();
  _sched_resume_next_thread ();
}

void _sched_resume_next_thread() {
  ASSERT_INTERRUPTS_DISABLED();  // Interrupts should be disabled at this point

  thread* current = wait_queue_head(readyq);

  if (current != NULL) {
    sched_current_thread = current;
    time now = current_time_no_interlock();
    current->_end_of_quantum = add_time(now, current->_quantum);
    _sched_set_timer(current->_end_of_quantum, now);
    restore_context(current->_sp); // never returns
  }

  panic(L"Deadlock detected");
}


#ifdef USE_PIT_FOR_TIMER

void irq0 ()
{
  ASSERT_INTERRUPTS_DISABLED ();

#ifdef SHOW_TIMER_INTERRUPTS
  term_write(cout, "\033[41m irq0 \033[0m");
#endif

  ACKNOWLEDGE_IRQ(0);

  _sched_timer_elapsed();
}

#endif

#ifdef USE_APIC_FOR_TIMER

void APIC_timer_irq ()
{
  ASSERT_INTERRUPTS_DISABLED ();

#ifdef SHOW_TIMER_INTERRUPTS
  term_write(cout, "\033[41m APIC timer irq \033[0m)");
#endif

  APIC_EOI = 0;

  _sched_timer_elapsed();
}

#endif


void sched_setup(void_fn continuation) {
   ASSERT_INTERRUPTS_DISABLED (); // Interrupts should be disabled at this point

  readyq = CAST(wait_queue*, kmalloc(sizeof(wait_queue)));
  wait_queue_init (readyq);

  sleepq = CAST(sleep_queue*, kmalloc(sizeof(sleep_queue)));
  sleep_queue_init (sleepq);

  thread* primordial = CAST(thread*, kmalloc(sizeof(thread)));

  sched_primordial_thread = new_thread(primordial, continuation, "The primordial");

  sched_current_thread = sched_primordial_thread;

  wait_queue_insert (sched_current_thread, readyq);

  _sched_setup_timer ();
  __asm__ __volatile__ ("int $0xD0":::"memory");
  // ** NEVER REACHED **
  panic(L"sched_setup should never return");
}

extern condvar* circular_buffer_cv;
extern rwmutex* m;

void sched_stats() {
  disable_interrupts();
// 
  // int n = 0;
  // int m = 0;
  // int p = 0;

  term_writeline(cout);
  term_write(cout, "Threads in wait queue:");
  {
    wait_mutex_node* t = readyq->super._next_in_wait_queue;
    while (t != &readyq->super) {
      term_write(cout, thread_name(CAST(thread*, t)));
      term_write(cout, " ");
      // n++;
      t = t->_next_in_wait_queue;
    }
  }

  term_writeline(cout);
  term_write(cout, "Threads in sleep queue:");
  {
    wait_mutex_sleep_node* t = sleepq->super._next_in_sleep_queue;
    while (t != &sleepq->super) {
      term_write(cout, thread_name(CAST(thread*, t)));
      term_write(cout, " ");
      // m++;
      t = t->_next_in_sleep_queue;
    }
  }

  term_writeline(cout);
  term_write(cout, "Threads in circular buffer condvar:");
  {
    wait_mutex_sleep_node* t = sleepq->super._next_in_sleep_queue;
    while (t != &sleepq->super) {
      term_write(cout, thread_name(CAST(thread*, t)));
      term_write(cout, " ");
      // m++;
      t = t->_next_in_sleep_queue;
    }
  }

  // term_writeline(cout);
  // term_write(cout, "RW mutex (->):");

  // {
  //   wait_mutex_node* t = m->super.super.super._next_in_wait_queue;
  //   while (t != &circular_buffer_cv->super.super) {
  //     term_write(cout, thread_name(CAST(thread*, t)));
  //     term_write(cout, " ");
  //     // n++;
  //     t = t->_next_in_wait_queue;
  //   }
  // }
  term_writeline(cout);

  term_write(cout, " ");

  // {
  //   for (int i = 0; i < mn; i++) {
  //     wait_mutex_node* t = &mtab[i]->super.super;
  //     while (t != &mtab[i]->super.super) {
  //       term_write(cout, "m");
  //       term_write(cout, i);
  //       term_write(cout, "=");
  //       term_write(cout, thread_name(CAST(thread*, t)));
  //       term_write(cout, " ");
  //       p++;
  //       t = t->_next_in_wait_queue;
  //     }
  //   }
  // }

  // {
  //   for (int i = 0; i < cn; i++) {
  //     wait_mutex_node* t = ctab[i]->super.super._next_in_wait_queue;
  //     while (t != &ctab[i]->super.super) {
  //       term_write(cout, "c");
  //       term_write(cout, i);
  //       term_write(cout, "=");
  //       term_write(cout, thread_name(CAST(thread*, t)));
  //       term_write(cout, " ");
  //       p++;
  //       t = t->_next_in_wait_queue;
  //     }
  //   }
  // }

  term_write(cout, ")\n");

  enable_interrupts();
}

void sched_reg_mutex(mutex* m) {
  mtab[mn++] = m;
}

void sched_reg_condvar(condvar* c) {
  ctab[cn++] = c;
}

void _sched_reschedule_thread(thread* t) {
  ASSERT_INTERRUPTS_DISABLED();  // Interrupts should be disabled at this point
  wait_queue_remove(t);
  wait_queue_insert(t, readyq);
}

void _sched_yield_if_necessary() {
  ASSERT_INTERRUPTS_DISABLED();  // Interrupts should be disabled at this point

  thread* t = wait_queue_head(readyq);

  if (t != sched_current_thread) {
    save_context(_sched_switch_to_next_thread, NULL);
  }
  
  ASSERT_INTERRUPTS_DISABLED();
}

void _sched_run_thread() {
  thread_run(sched_current_thread);
  sched_current_thread->_terminated = TRUE;
  condvar_broadcast(&sched_current_thread->_joiners);

  disable_interrupts();

  {
    wait_queue_remove(sched_current_thread);
    _sched_resume_next_thread();
  }

  // ** NEVER REACHED ** (this function never returns)
  panic(L"_sched_run_thread() should never return");
}

void _sched_switch_to_next_thread(uint32 cs, uint32 eflags, uint32* sp,
                                  void* q) {
  ASSERT_INTERRUPTS_DISABLED();  // Interrupts should be disabled at this point

  thread* current = sched_current_thread;
  
  current->_sp = sp;
  _sched_reschedule_thread(current);
  _sched_resume_next_thread();

  // ** NEVER REACHED ** (this function never returns)
  panic(L"_sched_switch_to_next_thread is never supposed to return");
}

void _sched_suspend_on_wait_queue(uint32 cs, uint32 eflags, uint32* sp,
                                  void* q) {
  ASSERT_INTERRUPTS_DISABLED();  // Interrupts should be disabled at this point

  thread* current = sched_current_thread;
  current->_sp = sp;

  wait_queue_remove(current);
  wait_queue_detach(current);

  wait_queue* wq = CAST(wait_queue*, q);

  wait_queue_insert(current, wq);

  _sched_resume_next_thread();

  // ** NEVER REACHED ** (this function never returns)
  panic(L"_sched_suspend_on_wait_queue is never supposed to return");
}

void _sched_suspend_on_sleep_queue(uint32 cs, uint32 eflags, uint32* sp,
                                   void* dummy) {
  ASSERT_INTERRUPTS_DISABLED();  // Interrupts should be disabled at this point

  thread* current = sched_current_thread;

  current->_sp = sp;
  sleep_queue_insert(current, sleepq);
  _sched_resume_next_thread();

  // ** NEVER REACHED ** (this function never returns)
  panic(L"_sched_suspend_on_sleep_queue is never supposed to return");
}

void _sched_setup_timer() {

  // When the timer elapses an interrupt is sent to the processor,
  // causing it to call the function "timer_elapsed".  This is how CPU
  // multiplexing is achieved.  Unfortunately, it takes quite a bit of
  // time to service an interrupt and this can be an important part of
  // the cost of a preemptive context switch on a fast machine.  On a
  // 400 MHz Pentium III based Compaq Presario 5830, each timer
  // interrupt takes about 900 to 1000 nanoseconds and a voluntary
  // context switch ("yield" with no timer reprogramming) takes about
  // 700 nanoseconds.

#ifdef USE_PIT_FOR_TIMER

#ifdef USE_PIT_1_BYTE_COUNT
#define PIT_COUNT_FORMAT PIT_CW_LSB
#else
#define PIT_COUNT_FORMAT PIT_CW_LSB_MSB
#endif

  outb(PIT_CW_CTR(0) | PIT_COUNT_FORMAT | PIT_CW_MODE(0),
       PIT_PORT_CW(PIT1_PORT_BASE));

  ENABLE_IRQ(0);

#endif

#ifdef USE_APIC_FOR_TIMER

  uint32 x;

  x = APIC_LVTT;
  x &= ~APIC_LVT_MASKED; // Unmask timer interrupt
  APIC_LVTT = x;

#endif
}

void _sched_set_timer(time t, time now) {
  // t must be >= now
  ASSERT_INTERRUPTS_DISABLED();

  int64 count;

#ifdef USE_PIT_FOR_TIMER

  count = time_to_pit_counts(subtract_time(t, now)) +
          2;  // 2 is added to avoid timer undershoot cascades when
              // PIT is running fast compared to RTC or TSC

  if (count > 0xffff) count = 0;
#ifdef USE_PIT_1_BYTE_COUNT
  else if (count > 0xff)
    count = 0xff;
#endif

  // The following "outb" instructions for sending the count to the
  // PIT are really slow and can be an important part of the cost of a
  // context switch on a fast machine.  On a 400 MHz Pentium III based
  // Compaq Presario 5830, each "outb" instruction takes about 900 to
  // 1000 nanoseconds and a voluntary context switch ("yield" with no
  // timer reprogramming) takes about 700 nanoseconds.

  outb(count, PIT_PORT_CTR(0, PIT1_PORT_BASE));  // send LSB

#ifndef USE_PIT_1_BYTE_COUNT
  outb(count >> 8, PIT_PORT_CTR(0, PIT1_PORT_BASE));  // send MSB
#endif

#endif

#ifdef USE_APIC_FOR_TIMER

  count = time_to_apic_timer_counts(subtract_time(t, now)) +
          100;  // 100 is added to avoid timer undershoot cascades when
                // APIC timer is running fast compared to RTC or TSC

  if (count > 0xffffffff) count = 0xffffffff;

  APIC_INITIAL_TIMER_COUNT = count;

#endif
}

extern void send_signal(int sig); // from libc/src/signal.c

void _sched_timer_elapsed() {

  ASSERT_INTERRUPTS_DISABLED ();

  time now = current_time_no_interlock ();

#if 1
  for (;;)
    {
      thread* t = sleep_queue_head (sleepq);

      if (t == NULL || less_time (now, t->_timeout)) {
        break;
      }

      t->_did_not_timeout = FALSE;
      sleep_queue_remove (t);
      sleep_queue_detach (t);
      _sched_reschedule_thread (t);
    }
#else
  {
    wait_mutex_sleep_node* temp = sleepq;
    while (temp->_next_in_sleep_queue != sleepq)
      {
        thread* t = CAST(thread*,temp);

        temp = temp->_next_in_sleep_queue;

        if (!less_time (now, t->_timeout))
          {
            t->_did_not_timeout = FALSE;
            sleep_queue_remove (t);
            sleep_queue_detach (t);
            reschedule_thread (t);
          }
      }
  }
#endif

  thread* current = sched_current_thread;

  if (less_time(now, current->_end_of_quantum)) {
    //      cout << "timer is fast\n";/////////////
    _sched_set_timer(current->_end_of_quantum, now);
  } else {
    send_signal(26); // send SIGVTALRM
    save_context(_sched_switch_to_next_thread, NULL);
  }
}

//-----------------------------------------------------------------------------

// mode: C++ //
// End: //

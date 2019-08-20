// file: "thread.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Oct 01  initial version (Marc Feeley)

#ifndef THREAD_H
#define THREAD_H

//-----------------------------------------------------------------------------

#include "chrono.h"
#include "general.h"
#include "intr.h"

//-----------------------------------------------------------------------------

const uint32 GAMBIT_START = 0x100000;

//-----------------------------------------------------------------------------

#define STI()                                   \
  do {                                          \
    __asm__ __volatile__("sti" : : : "memory"); \
  } while (0)
#define CLI()                                   \
  do {                                          \
    __asm__ __volatile__("cli" : : : "memory"); \
  } while (0)

//-----------------------------------------------------------------------------

#ifdef CHECK_ASSERTIONS

#define ARE_INTERRUPTS_ENABLED() (eflags_reg() & (1 << 9))

#define ASSERT_INTERRUPTS_DISABLED()                 \
  do {                                               \
    if (ARE_INTERRUPTS_ENABLED() != 0) {             \
      debug_write(__FILE__);                         \
      debug_write(":");                              \
      debug_write(__LINE__);                         \
      panic(L"FAILED ASSERT_INTERRUPTS_DISABLED\n"); \
    }                                                \
  } while (0)

#define ASSERT_INTERRUPTS_ENABLED()                 \
  do {                                              \
    if (!ARE_INTERRUPTS_ENABLED()) {                \
      debug_write(__FILE__);                        \
      debug_write(":");                             \
      debug_write(__LINE__);                        \
      panic(L"FAILED ASSERT_INTERRUPTS_ENABLED\n"); \
    }                                               \
  } while (0)

#else

#ifdef PRINT_ASSERTIONS
#define ASSERT_INTERRUPTS_DISABLED()             \
  do {                                           \
    debug_write("Failed interrupt disabled at"); \
    debug_write(__LINE__);                       \
    debug_write(__FILE__);                       \
    debug_write("----------------------------"); \
  } while (0)

#define ASSERT_INTERRUPTS_ENABLED()              \
  do {                                           \
    debug_write("Failed interrupt enabled at");  \
    debug_write(__LINE__);                       \
    debug_write(__FILE__);                       \
    debug_write("----------------------------"); \
  } while (0)

#else
#define ASSERT_INTERRUPTS_DISABLED()
#define ASSERT_INTERRUPTS_ENABLED()

#endif

#endif

#undef disable_interrupts

#define disable_interrupts()     \
  do {                           \
    ASSERT_INTERRUPTS_ENABLED(); \
    CLI();                       \
  } while (0)

#undef enable_interrupts

#define enable_interrupts()       \
  do {                            \
    ASSERT_INTERRUPTS_DISABLED(); \
    STI();                        \
  } while (0)

// Save and restore the CPU state.

#if 1

#define save_context(receiver, data)                                            \
  do {                                                                          \
    ASSERT_INTERRUPTS_DISABLED();                                               \
    __asm__ __volatile__(                                                       \
        "pusha                                                             \n \
         pushl %1               # The fourth parameter of the receiver fn  \n \
         lea   -16(%%esp),%%eax # The third parameter of the receiver fn   \n \
         pushl %%eax                                                       \n \
         pushfl                 # Setup a stack frame with the same format \n \
         pushl %%cs             #  as expected by the ``iret'' instruction \n \
         call  %P0              #  so that ``iret'' can restore the context\n \
         addl  $8,%%esp         # Remove the third and fourth parameter    \n \
         popa" \
        :                                                                       \
        : "i"(receiver), "g"(data)                                              \
        : "memory");                                                            \
  } while (0)

#else

#define save_context(receiver, data)                                            \
  do {                                                                          \
    ASSERT_INTERRUPTS_DISABLED();                                               \
    __asm__ __volatile__(                                                       \
        "pushl %%eax                                                       \n \
         pushl %%ebx                                                       \n \
         pushl %%ecx                                                       \n \
         pushl %%edx                                                       \n \
         pushl %%esi                                                       \n \
         pushl %%edi                                                       \n \
         pushl %%ebp                                                       \n \
         pushl %1               # The fourth parameter of the receiver fn  \n \
         lea   -16(%%esp),%%eax # The third parameter of the receiver fn   \n \
         pushl %%eax                                                       \n \
         pushfl                 # Setup a stack frame with the same format \n \
         pushl %%cs             #  as expected by the ``iret'' instruction \n \
         call  %c0              #  so that ``iret'' can restore the context\n \
         addl  $8,%%esp         # Remove the third and fourth parameter    \n \
         popl  %%ebp                                                       \n \
         popl  %%edi                                                       \n \
         popl  %%esi                                                       \n \
         popl  %%edx                                                       \n \
         popl  %%ecx                                                       \n \
         popl  %%ebx                                                       \n \
         popl  %%eax" \
        :                                                                       \
        : "i"(receiver), "g"(data)                                              \
        : "memory");                                                            \
  } while (0)

#endif

#ifdef USE_IRET_FOR_RESTORE_CONTEXT

#define restore_context(sp)                                                     \
  do {                                                                          \
    ASSERT_INTERRUPTS_DISABLED();                                               \
    __asm__ __volatile__(                                                       \
        "movl  %0,%%esp  # Restore the stack pointer                       \n \
         movl  %0,%%esp  # Restore the stack pointer                       \n \
         iret            # Return from the ``call'' in ``save_context''" \
        :                                                                       \
        : "g"(sp));                                                             \
  } while (0)

#endif

#ifdef USE_RET_FOR_RESTORE_CONTEXT

#define restore_context(sp)                                                     \
  do {                                                                          \
    ASSERT_INTERRUPTS_DISABLED();                                               \
    __asm__ __volatile__(                                                       \
        "movl  %0,%%esp  # Restore the stack pointer                       \n \
         popl  %%ebx     # Get return address                              \n \
         popl  %%eax     # Discard %%cs                                    \n \
         popfl                                                                \n\
         jmp   *%%ebx    # Return from the ``call'' in ``save_context''" \
        :                                                                       \
        : "g"(sp));                                                             \
  } while (0)

#endif

//-----------------------------------------------------------------------------

// Available thread priorities.

typedef int priority;

#define null_priority 0
#define low_priority 100
#define normal_priority 200
#define high_priority 300

#define THREAD_TYPE_KERNEL 1
#define THREAD_TYPE_USER 2

//-----------------------------------------------------------------------------

#define protected public  //////////////////

// Select implementations.

#define USE_DOUBLY_LINKED_LIST_FOR_WAIT_QUEUE
//#define USE_RED_BLACK_TREE_FOR_WAIT_QUEUE

#define USE_DOUBLY_LINKED_LIST_FOR_MUTEX_QUEUE
//#define USE_RED_BLACK_TREE_FOR_MUTEX_QUEUE

#define USE_DOUBLY_LINKED_LIST_FOR_SLEEP_QUEUE
//#define USE_RED_BLACK_TREE_FOR_SLEEP_QUEUE

typedef void (*void_fn)();
typedef int (*libc_startup_fn)(int argc, char* argv[], char* env[]);
//-----------------------------------------------------------------------------

#if 0
// "pthread" class declaration.

class pthread : public thread
  {
  public:

    pthread (void_fn run);
    virtual ~pthread ();

  protected:

    void* (*_start_routine) (void*);
    void* _retval;

    static void run ();
  };

typedef pthread* pthread_t;

typedef int pthread_attr_t; // dummy declaration

int pthread_create (pthread_t* thread,
                    pthread_attr_t* attr,
                    void* (*start_routine) (void*),
                    void* arg);
int pthread_join (pthread_t thread,
                  void** retval);
void pthread_exit (void* retval);
int pthread_equal (pthread_t thread1, pthread_t thread2);
pthread_t pthread_self ();

typedef mutex* pthread_mutex_t;

typedef int pthread_mutexattr_t; // dummy declaration

int pthread_mutex_init (pthread_mutex_t* mutex,
                        const pthread_mutexattr_t* mutexattr);
int pthread_mutex_lock (pthread_mutex_t* mutex);
int pthread_mutex_trylock (pthread_mutex_t* mutex);
int pthread_mutex_unlock (pthread_mutex_t* mutex);
int pthread_mutex_destroy (pthread_mutex_t* mutex);

typedef condvar* pthread_cond_t;

typedef int pthread_condattr_t; // dummy declaration

int pthread_cond_init (pthread_cond_t* cond,
                       pthread_condattr_t* cond_attr);
int pthread_cond_signal (pthread_cond_t* cond);
int pthread_cond_broadcast (pthread_cond_t* cond);
int pthread_cond_wait (pthread_cond_t* cond,
                       pthread_mutex_t *mutex);
int pthread_cond_timedwait (pthread_cond_t* cond,
                            pthread_mutex_t* mutex,
                            const struct timespec* time);
int pthread_cond_destroy (pthread_cond_t* cond);

#endif

//-----------------------------------------------------------------------------
// C REWRITE
//-----------------------------------------------------------------------------

struct wait_queue;
struct mutex_queue;
struct sleep_queue;

typedef struct wait_mutex_node {
  // Wait queue part for maintaining the set of threads waiting on a
  // synchronization object or the run queue.  If this object is a
  // thread it is an element of the queue; if this object is a
  // synchronization object or the run queue it is the queue of
  // waiting threads.

#ifdef USE_DOUBLY_LINKED_LIST_FOR_WAIT_QUEUE
  wait_mutex_node* volatile _next_in_wait_queue;
  wait_mutex_node* volatile _prev_in_wait_queue;
#endif

#ifdef USE_RED_BLACK_TREE_FOR_WAIT_QUEUE
  wait_queue* volatile _color_in_wait_queue;
  wait_mutex_node* volatile _parent_in_wait_queue;
  wait_mutex_node* volatile _left_in_wait_queue;
#endif

  // Mutex queue part for maintaining the set of mutexes owned by a
  // thread.  If this object is a mutex it is a queue element; if
  // this object is a thread it is the queue of owned mutexes.

#ifdef USE_DOUBLY_LINKED_LIST_FOR_MUTEX_QUEUE
  wait_mutex_node* volatile _next_in_mutex_queue;
  wait_mutex_node* volatile _prev_in_mutex_queue;
#endif

#ifdef USE_RED_BLACK_TREE_FOR_MUTEX_QUEUE
  mutex_queue* volatile _color_in_mutex_queue;
  wait_mutex_node* volatile _parent_in_mutex_queue;
  wait_mutex_node* volatile _left_in_mutex_queue;
#endif
} wait_mutex_node;

typedef struct wait_queue {
  wait_mutex_node super;

#ifdef USE_DOUBLY_LINKED_LIST_FOR_WAIT_QUEUE
#endif

#ifdef USE_RED_BLACK_TREE_FOR_WAIT_QUEUE
  wait_mutex_node* volatile _leftmost_in_wait_queue;
#endif
} wait_queue;

typedef struct mutex_queue {
  wait_mutex_node super;

#ifdef USE_DOUBLY_LINKED_LIST_FOR_MUTEX_QUEUE
#endif

#ifdef USE_RED_BLACK_TREE_FOR_MUTEX_QUEUE
  wait_mutex_node* volatile _leftmost_in_mutex_queue;
#endif
} mutex_queue;

typedef struct wait_mutex_sleep_node {
  mutex_queue super;

 protected:
  // Sleep queue part for maintaining the set of threads waiting for
  // a timeout.  If this object is a thread it is an element of the
  // queue; if this object is the run queue it is the queue of
  // threads waiting for a timeout.

#ifdef USE_DOUBLY_LINKED_LIST_FOR_SLEEP_QUEUE
  wait_mutex_sleep_node* volatile _next_in_sleep_queue;
  wait_mutex_sleep_node* volatile _prev_in_sleep_queue;
#endif

#ifdef USE_RED_BLACK_TREE_FOR_SLEEP_QUEUE
  sleep_queue* volatile _color_in_sleep_queue;
  wait_mutex_sleep_node* volatile _parent_in_sleep_queue;
  wait_mutex_sleep_node* volatile _left_in_sleep_queue;
#endif
} wait_mutex_sleep_node;

typedef struct sleep_queue {
  wait_mutex_sleep_node super;

 protected:
#ifdef USE_DOUBLY_LINKED_LIST_FOR_SLEEP_QUEUE
#endif

#ifdef USE_RED_BLACK_TREE_FOR_SLEEP_QUEUE
  wait_mutex_sleep_node* volatile _leftmost_in_sleep_queue;
#endif
} sleep_queue;

typedef struct mutex {
  wait_queue super;
  // The inherited "wait queue" part of wait_queue is used to
  // maintain the set of threads waiting on this mutex.

  // The inherited "mutex queue" part of wait_queue is used to
  // maintain this mutex on the mutex_queue of the thread that owns
  // it.

  volatile bool _locked;  // boolean indicating if mutex is locked or unlocked
} mutex;

typedef struct rwmutex {
  mutex super;
  volatile uint16 _readers;
  volatile uint16 _writerq;
} rwmutex;

mutex* new_mutex(mutex* m);

rwmutex* new_rwmutex(rwmutex* rwm);

void mutex_lock(mutex* self);

bool mutex_lock_or_timeout(mutex* self, time timeout);

void mutex_unlock(mutex* self);

void rwmutex_readlock(rwmutex* self);

void rwmutex_writelock(rwmutex* self);

void rwmutex_readunlock(rwmutex* self);

void rwmutex_writeunlock(rwmutex* self);

extern mutex* seq;

typedef struct condvar {
  wait_queue super;
  // The inherited "wait queue" part of wait_queue is used to
  // maintain the set of threads waiting on this condvar.

  // The inherited "mutex queue" part of wait_queue is unused.
} condvar;

condvar* new_condvar(condvar* t);

void condvar_wait(
    condvar* self,
    mutex* m);  // suspends current thread on the condition variable

bool condvar_wait_or_timeout(condvar* self, mutex* m,
                             time timeout);  // returns FALSE on timeout

void condvar_signal(condvar* self);     // resumes one of the waiting threads
void condvar_broadcast(condvar* self);  // resumes all of the waiting threads

void condvar_mutexless_wait(
    condvar* self);  // like "wait" but uses interrupt flag as mutex
void condvar_mutexless_signal(
    condvar* self);  // like "signal" but assumes disabled interrupts

typedef uint8 thread_type;

typedef struct thread;

typedef struct thread_vtable_struct {
  void (*thread_run)(thread* self);
} thread_vtable;

typedef struct thread {
  wait_mutex_sleep_node super;
  thread_type type;
  thread_vtable_struct* vtable;
#ifdef USE_DOUBLY_LINKED_LIST_FOR_WAIT_QUEUE
#endif

#ifdef USE_RED_BLACK_TREE_FOR_WAIT_QUEUE
  wait_mutex_node* volatile _right_in_wait_queue;
#endif

#ifdef USE_DOUBLY_LINKED_LIST_FOR_SLEEP_QUEUE
#endif

#ifdef USE_RED_BLACK_TREE_FOR_SLEEP_QUEUE
  wait_mutex_sleep_node* volatile _right_in_sleep_queue;
#endif

  uint32* _stack;  // the thread's stack
  uint32* _sp;     // the thread's stack pointer

  time _quantum;         // duration of the quantum for this thread
  time _end_of_quantum;  // moment in time when current quantum ends

  time _timeout;          // when to end sleeping
  bool _did_not_timeout;  // to tell if synchronization operation timed out

  int _prio;  // the thread's priority

  mutex _m;                   // mutex to access termination flag
  condvar _joiners;           // threads waiting for this thread to terminate
  volatile bool _terminated;  // the thread's termination flag
  native_string _name;
  void_fn _run;
} thread;

typedef struct program_thread_struct {
  thread super;
  libc_startup_fn _code;
  native_string _cwd;
} program_thread;

thread* new_thread(thread* self, void_fn run, native_string name);

thread* thread_start(thread* self);

program_thread* new_program_thread(program_thread* self, native_string cwd,
                                   libc_startup_fn run, native_string name);

native_string program_thread_cwd(program_thread* self);
native_string program_thread_chdir(program_thread* self, native_string new_cwd);

void thread_join(thread* self);

void thread_yield();

thread* thread_self();

void thread_sleep(uint64 timeout_nsecs);

native_string thread_name(thread* self);

void virtual_thread_run(thread* self);

void virtual_program_thread_run(thread* self);

void sched_setup(void_fn cont);

void sched_stats();

void sched_reg_mutex(mutex* m);

void sched_reg_condvar(condvar* c);

void _sched_reschedule_thread(thread* t);

void _sched_yield_if_necessary();

void _sched_run_thread();

void _sched_switch_to_next_thread(uint32 cs, uint32 eflags, uint32* sp,
                                  void* dummy);

void _sched_suspend_on_wait_queue(uint32 cs, uint32 eflags, uint32* sp,
                                  void* dummy);

void _sched_suspend_on_sleep_queue(uint32 cs, uint32 eflags, uint32* sp,
                                   void* dummy);

void _sched_setup_timer();

void _sched_set_timer(time t, time now);

void _sched_timer_elapsed();

void _sched_resume_next_thread();

void sys_irq(void* esp);

#ifdef USE_PIT_FOR_TIMER

void irq0();

#endif

#ifdef USE_APIC_FOR_TIMER

void APIC_timer_irq();

#endif

//-----------------------------------------------------------------------------

// "wait_mutex_node" class implementation.

#define NODETYPE wait_mutex_node
#define QUEUETYPE wait_queue
#define ELEMTYPE thread
#define NAMESPACE_PREFIX(name) wait_queue_##name
#define BEFORE(elem1, elem2) ((elem1)->_prio > (elem2)->_prio)

#ifdef USE_DOUBLY_LINKED_LIST_FOR_WAIT_QUEUE
#define USE_DOUBLY_LINKED_LIST
#define NEXT(node) (node)->_next_in_wait_queue
#define NEXT_SET(node, next_node) NEXT(node) = (next_node)
#define PREV(node) (node)->_prev_in_wait_queue
#define PREV_SET(node, prev_node) PREV(node) = (prev_node)
#include "queue.h"
#undef USE_DOUBLY_LINKED_LIST
#undef NEXT
#undef NEXT_SET
#undef PREV
#undef PREV_SET
#endif

#ifdef USE_RED_BLACK_TREE_FOR_WAIT_QUEUE
#define USE_RED_BLACK_TREE
#define COLOR(node) (node)->_color_in_wait_queue
#define COLOR_SET(node, color) COLOR(node) = (color)
#define PARENT(node) (node)->_parent_in_wait_queue
#define PARENT_SET(node, parent) PARENT(node) = (parent)
#define LEFT(node) (node)->_left_in_wait_queue
#define LEFT_SET(node, left) LEFT(node) = (left)
#define RIGHT(node) CAST(ELEMTYPE*, node)->_right_in_wait_queue
#define RIGHT_SET(node, right) RIGHT(node) = (right)
#define LEFTMOST(queue) (queue)->_leftmost_in_wait_queue
#define LEFTMOST_SET(queue, node) LEFTMOST(queue) = (node)
#include "queue.h"
#undef USE_RED_BLACK_TREE
#undef COLOR
#undef COLOR_SET
#undef PARENT
#undef PARENT_SET
#undef LEFT
#undef LEFT_SET
#undef RIGHT
#undef RIGHT_SET
#undef LEFTMOST
#undef LEFTMOST_SET
#endif

#undef NODETYPE
#undef QUEUETYPE
#undef ELEMTYPE
#undef NAMESPACE_PREFIX
#undef BEFORE

//-----------------------------------------------------------------------------

// "wait_mutex_node" class implementation.

#define NODETYPE wait_mutex_node
#define QUEUETYPE mutex_queue
#define ELEMTYPE mutex
#define NAMESPACE_PREFIX(name) mutex_queue_##name
#define BEFORE(elem1, elem2) FALSE

#ifdef USE_DOUBLY_LINKED_LIST_FOR_MUTEX_QUEUE
#define USE_DOUBLY_LINKED_LIST
#define NEXT(node) (node)->_next_in_mutex_queue
#define NEXT_SET(node, next_node) NEXT(node) = (next_node)
#define PREV(node) (node)->_prev_in_mutex_queue
#define PREV_SET(node, prev_node) PREV(node) = (prev_node)
#include "queue.h"
#undef USE_DOUBLY_LINKED_LIST
#undef NEXT
#undef NEXT_SET
#undef PREV
#undef PREV_SET
#endif

#ifdef USE_RED_BLACK_TREE_FOR_MUTEX_QUEUE
#include "queue.h"
#endif

#undef NODETYPE
#undef QUEUETYPE
#undef ELEMTYPE
#undef NAMESPACE_PREFIX
#undef BEFORE

//-----------------------------------------------------------------------------

// "wait_mutex_sleep_node" class implementation.

#define NODETYPE wait_mutex_sleep_node
#define QUEUETYPE sleep_queue
#define ELEMTYPE thread
#define NAMESPACE_PREFIX(name) sleep_queue_##name
#define BEFORE(elem1, elem2) less_time((elem1)->_timeout, (elem2)->_timeout)

#ifdef USE_DOUBLY_LINKED_LIST_FOR_SLEEP_QUEUE
#define USE_DOUBLY_LINKED_LIST
#define NEXT(node) (node)->_next_in_sleep_queue
#define NEXT_SET(node, next_node) NEXT(node) = (next_node)
#define PREV(node) (node)->_prev_in_sleep_queue
#define PREV_SET(node, prev_node) PREV(node) = (prev_node)
#include "queue.h"
#undef USE_DOUBLY_LINKED_LIST
#undef NEXT
#undef NEXT_SET
#undef PREV
#undef PREV_SET
#endif

#ifdef USE_RED_BLACK_TREE_FOR_SLEEP_QUEUE
#include "queue.h"
#endif

#undef NODETYPE
#undef QUEUETYPE
#undef ELEMTYPE
#undef NAMESPACE_PREFIX
#undef BEFORE

//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------
// Static declarations
//-----------------------------------------------------------------------

extern wait_queue* readyq;
extern sleep_queue* sleepq;
extern thread* sched_primordial_thread;
extern thread* sched_current_thread;
extern thread_vtable _thread_vtable;
extern thread_vtable _program_thread_vtable;

#endif

// Local Variables: //
// mode: C++ //
// End: //

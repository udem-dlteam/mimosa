#include "include/libc_common.h"
#include "include/setjmp.h"

int REDIRECT_NAME(setjmp)(jmp_buf __env) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._setjmp(__env);


  // generates the code:
  //
  // setjmp:
  //         pushl   %ebp
  //         movl    %esp, %ebp
  //         subl    $8, %esp
  //         movl    8(%ebp), %eax
  //         movl    %eax, (%esp)
  //         movl    127096, %eax
  //         call    *%eax
  //         leave
  //         ret
  //
  // the stack will have this layout:
  //
  //    |           |
  //    +-----------+
  //    |   __env   | __env parameter of setjmp
  //    +-----------+
  //    |  ret adr  | <-- esp when setjmp is entered
  //    +-----------+
  //    | saved ebp | <-- ebp
  //    +-----------+
  //    | NOT_USED  |
  //    +-----------+
  //    |   __env   | __env parameter of _setjmp
  //    +-----------+
  //    |  ret adr  | <-- esp when _setjmp is entered
  //    +-----------+

#else

  libc_trace("setjmp");

#ifdef USE_HOST_LIBC

  return setjmp(__env);

#else

  __asm__ __volatile__ ("\
                                                        \n\
#    movl %ebp,%esp    # undo saving of ebp              \n\
#    popl %ebp                                           \n\
    movl %ebp,%esp    # undo saving of ebp              \n\
    popl %ebp                                           \n\
                                                        \n\
    movl 0(%esp),%ecx # get return address              \n\
    movl 4(%esp),%edx # get __env                       \n\
    movl %ecx,0(%edx) # save return address in __env    \n\
    movl %esp,4(%edx) # save esp in __env               \n\
    movl %ebp,8(%edx) # save ebp in __env               \n\
    movl %ebx,12(%edx) # save callee save regs in __env \n\
    movl %esi,16(%edx)                                  \n\
    movl %edi,20(%edx)                                  \n\
                                                        \n\
    movl $0,%eax       # return 0                       \n\
    ret                                                 \n\
                                                        \n\
    ");

  return 0; // keep compiler happy

#endif
#endif
}

void REDIRECT_NAME(longjmp)(jmp_buf __env, int __val) {

#ifdef USE_LIBC_LINK

  LIBC_LINK._longjmp(__env, __val);

#else

  libc_trace("longjmp");

#ifdef USE_HOST_LIBC

  exit(0); // don't actually longjmp, because setjmp definition above is bogus
  longjmp(__env, __val);

#else

  __asm__ __volatile__ ("\
                                                             \n\
#    movl %ebp,%esp    # undo saving of ebp                   \n\
#    popl %ebp                                                \n\
                                                             \n\
    movl 4(%esp),%edx # get __env                            \n\
    movl 8(%esp),%eax # get __val                            \n\
    movl 0(%edx),%ecx # get return address from __env        \n\
    movl 4(%edx),%esp # restore esp from __env               \n\
    movl %ecx,0(%esp) # restore return address               \n\
    movl %edx,4(%esp) # restore __env (just in case)         \n\
    movl 8(%edx),%ebp # restore ebp from __env               \n\
    movl 12(%edx),%ebx # restore callee save regs from __env \n\
    movl 16(%edx),%esi                                       \n\
    movl 20(%edx),%edi                                       \n\
                                                             \n\
    ret                                                      \n\
                                                             \n\
    ");

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_setjmp(void) {
}

#endif

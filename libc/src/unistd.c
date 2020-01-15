#include "include/libc_common.h"
#include "include/unistd.h"
#include "include/errno.h"

#ifdef USE_MIMOSA

#include "thread.h"
#include "../drivers/filesystem/include/vfs.h"

#endif

int REDIRECT_NAME(chdir)(const char *__path) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._chdir(__path);

#else

  libc_trace("chdir");

#ifdef USE_HOST_LIBC

  return chdir(__path);

#else

  //  debug_write("chdir");
  //  debug_write(CAST(native_string, __path));

  thread* gamb_thread = thread_self();

  error_code err;
  stat_buff sbuffer;

  if (ERROR(err = file_stat(CAST(native_string, __path), &sbuffer))) {

    switch (err) {
      case FNF_ERROR:
        errno = ENOENT;
        break;

      default:
        errno = ENOENT;
        break;
    }

    return -1;
  }

  if ((sbuffer.type & TYPE_FOLDER) != TYPE_FOLDER) {
    errno = ENOTDIR;
    return -1;
  }

  if(gamb_thread->type == THREAD_TYPE_USER) {
    program_thread* t = CAST(program_thread*, gamb_thread);
    program_thread_chdir(t, CAST(native_string, __path));
  } else {
    panic(L"Gambit thread is a kernel thread");
  }

  return 0;

#endif
#endif
}

char *REDIRECT_NAME(getcwd)(char *__buf, size_t __size) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._getcwd(__buf, __size);

#else

  libc_trace("getcwd");

#ifdef USE_HOST_LIBC

  return getcwd(__buf, __size);

#else

  thread* gamb_thread = thread_self();

  if(gamb_thread->type == THREAD_TYPE_USER) {
    program_thread* t = CAST(program_thread*, gamb_thread);
    native_string cwd = t->_cwd;
    char *p = __buf;
    while ((*p++ = *cwd++) != '\0') ;
    return __buf;
  } else {
    panic(L"Gambit thread is a kernel thread");
  }

  return NULL;

#endif
#endif
}

int REDIRECT_NAME(mkdir)(const char *__pathname, mode_t __mode) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._mkdir(__pathname, __mode);

#else

  libc_trace("mkdir");

#ifdef USE_HOST_LIBC

  return mkdir(__pathname, __mode);

#else
  file* new_folder;
  error_code err = mkdir(CAST(native_string, __pathname), &new_folder);
  if (HAS_NO_ERROR(err)) file_close(new_folder);

  return ERROR(err) ? -1 : 0;
#endif
#endif
}

int REDIRECT_NAME(remove)(const char *__pathname) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._remove(__pathname);

#else

  libc_trace("remove");

#ifdef USE_HOST_LIBC

  return remove(__pathname);

#else
  error_code err = NO_ERROR; 
  
  err = file_remove(CAST(native_string, __pathname));

  if(ERROR(err)) {
    switch (err)
    {
    case FNF_ERROR:
      errno = ENOENT;
      break;
    default:
      errno = ENOENT;
      break;
    }
  }

  return ERROR(err) ? -1 : 0;

#endif
#endif
}

int REDIRECT_NAME(lstat)(const char *__pathname, struct stat *__buf) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._lstat(__pathname, __buf);

#else

  libc_trace("lstat");

#ifdef USE_HOST_LIBC

  return lstat(__pathname, __buf);

#else
  return REDIRECT_NAME(stat)(__pathname, __buf);
#endif
#endif
}

int REDIRECT_NAME(stat)(const char *__pathname, struct stat *__buf) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._stat(__pathname, __buf);

#else

  libc_trace("stat");

#ifdef USE_HOST_LIBC

  return stat(__pathname, __buf);

#else

  //  debug_write("stat");
  //  debug_write(CAST(native_string, __pathname));

  error_code err;
  stat_buff sbuffer;
  if(ERROR(err = file_stat(CAST(native_string, __pathname), &sbuffer))) {

    switch (err)
    {
    case FNF_ERROR:
      errno = ENOENT; 
      break;
    
    default:
      errno = ENOENT;
      break;
    }

    return -1;
  }

  __buf->st_gid = __buf->st_uid = 0;
  __buf->st_nlink = 0;
  __buf->st_size = sbuffer.bytes;
  __buf->st_dev = CAST(uint32, sbuffer.fs);
  __buf->st_atim.ts_sec = sbuffer.last_modifs_epochs_secs;
  __buf->st_mtim.ts_sec = sbuffer.last_modifs_epochs_secs;
  __buf->st_ctim.ts_sec = sbuffer.creation_time_epochs_secs;

  __buf->st_atim.ts_nsec = __buf->st_ctim.ts_nsec = __buf->st_mtim.ts_nsec = 0;
  
  __buf->st_mode = 0;

  if ((sbuffer.type & TYPE_FOLDER) == TYPE_FOLDER) {
    __buf->st_mode |= S_IFDIR;
  }
  
  if ((sbuffer.type & TYPE_REGULAR) == TYPE_REGULAR) {
    __buf->st_mode |= S_IFREG;
  }

  if ((sbuffer.type & TYPE_VFILE) == TYPE_VFILE) {
    __buf->st_mode |= S_IFSOCK;
  }

  if((sbuffer.type & TYPE_MOUNTPOINT) == TYPE_MOUNTPOINT) {
    __buf->st_mode |= S_IFBLK;
  }

  __buf->st_mode |= 0x777; // All permissions
  
  return 0;
#endif
#endif
}

int REDIRECT_NAME(isatty)(int __fd) {

#ifdef USE_LIBC_LINK

  return LIBC_LINK._isatty(__fd);

#else

  libc_trace("isatty");

#ifdef USE_HOST_LIBC

  return isatty(__fd);

#else

  // TODO: implement
  return 0;

#endif
#endif
}

#ifndef USE_LIBC_LINK

void libc_init_unistd(void) {
}

#endif

#include "include/stdstream.h"
#include "general.h"
#include "include/vfs.h"
#include "rtlib.h"
#include "thread.h"

#define STREAM_DEFAULT_LEN 512

native_string STDIN_PATH = "/sys/stdin";
native_string STDOUT_PATH = "/sys/stdout";

static native_string STDIN_PART = "STDIN";
static native_string STDOUT_PART = "STDOUT";

static fs_header fs_std_stream;

static raw_stream stdin;
static raw_stream stdout;

static file_vtable __std_rw_file_stream_vtable;
static fs_vtable __std_stream_vtable;

static error_code new_raw_stream(raw_stream* rs, bool autoresize);
static error_code new_stream_file(stream_file* rs, file_mode mode,
                                  raw_stream* source);

static void stream_reset_cursor(file* f);
static error_code stream_move_cursor(file* f, int32 n);
static error_code stream_set_to_absolute_position(file* f, uint32 position);
static size_t stream_len(file* f);

static error_code stream_close(file* f);
static error_code stream_write(file* f, void* buff, uint32 count);
static error_code stream_read(file* f, void* buf, uint32 count);

// -------------------------------------------------------------
// Methods that don't make sense on a stream
// -------------------------------------------------------------
void stream_reset_cursor(file* f) { return; }

static error_code stream_move_cursor(file* f, int32 n) { return ARG_ERROR; }

static error_code stream_set_to_absolute_position(file* f, uint32 position) {
  return ARG_ERROR;
}

static size_t stream_len(file* f) { return 0; }

error_code stream_mkdir(fs_header* header, short_file_name* parts, uint8 depth, file**result) {
  return ARG_ERROR;
}

error_code stream_rename(fs_header* header, file* source, short_file_name* parts, uint8 depth) {
  return ARG_ERROR;
}

error_code stream_remove(fs_header* header, file* source) {
  return ARG_ERROR;
}

// -------------------------------------------------------------
// Stream management
// -------------------------------------------------------------

static error_code new_raw_stream(raw_stream* rs, bool autoresize) {
  if (NULL == rs) return ARG_ERROR;

  error_code err = NO_ERROR;
  rs->len = STREAM_DEFAULT_LEN;
  rs->buff = kmalloc(sizeof(uint8) * rs->len);

  if (NULL == rs->buff) return MEM_ERROR;

  rs->autoresize = autoresize;
  rs->high = 0;

  condvar* readycv = CAST(condvar*, kmalloc(sizeof(condvar)));

  if (NULL == readycv) {
    kfree(rs->buff);
    return MEM_ERROR;
  }

  rs->readycv = new_condvar(readycv);

  return err;
}

static error_code new_stream_file(stream_file* rs, file_mode mode,
                                  raw_stream* source) {
  error_code err = NO_ERROR;

  if (NULL == rs) {
    return ARG_ERROR;
  }

  if (NULL == source) {
    return ARG_ERROR;
  }

  bool write_only;

  if (IS_MODE_WRITE_ONLY(mode)) {
    write_only = TRUE;
  } else {
    write_only = FALSE;
  }

  rs->header.mode = mode;
  rs->header._vtable = &__std_rw_file_stream_vtable;
  rs->_source = source;
  rs->_lo = 0;
  rs->_reset = source->_reset;

  if (!write_only) source->readers++;

  return err;
}

static error_code stream_close(file* ff) {
  error_code err = NO_ERROR;
  stream_file* f = CAST(stream_file*, ff);
  kfree(f);
  return err;
}

#define stream_reset(rs)                     \
  do {                                       \
    (rs)->_reset = !(rs)->_reset;            \
    (rs)->high = 0;                          \
    condvar_mutexless_signal((rs)->readycv); \
  } while (0);

static error_code stream_write(file* ff, void* buff, uint32 count) {
  // if (!IS_MODE_WRITE(ff->type)) return PERMISSION_ERROR;

  error_code err = NO_ERROR;
  stream_file* f = CAST(stream_file*, ff);
  raw_stream* rs = f->_source;
  condvar* streamcv = rs->readycv;
  uint8* stream_buff = CAST(uint8*, rs->buff);
  uint8* source_buff = CAST(uint8*, buff);

  bool inter_disabled = ARE_INTERRUPTS_ENABLED();

  {
    if (inter_disabled) disable_interrupts();

    // Loop like this and do not use mem copy because
    // we want to signal every new character)
    uint32 i;
    for (i = 0; i < count; /*++i is in the write branch*/) {
      int next_hi = (rs->high + 1);
      if (next_hi < rs->len) {
        stream_buff[rs->high] = source_buff[i];
        rs->high = next_hi;
        ++i;
        // No one is caught up: there's a new char
        rs->late = rs->readers;
        condvar_mutexless_signal(streamcv);
      } else if (rs->late == 0) { // this allows a not reader stream to never overflow
        stream_reset(rs);
      } else if (rs->autoresize) {
        // Resize
        panic(L"STD stream resize not implemented yet");
      } else {
        err = MEM_ERROR;
        break;
      }
    }

    if (HAS_NO_ERROR(err) && i == count) {
      err = count;
    }

    if (inter_disabled) enable_interrupts();
  }

  return err;
}

static error_code stream_read(file* ff, void* buff, uint32 count) {
  error_code err = NO_ERROR;
  stream_file* f = CAST(stream_file*, ff);
  raw_stream* rs = f->_source;
  condvar* streamcv = rs->readycv;
  uint8* stream_buff = CAST(uint8*, rs->buff);
  uint8* read_buff = CAST(uint8*, buff);

  bool inter_disabled = ARE_INTERRUPTS_ENABLED();

  if (inter_disabled) disable_interrupts();

  if (f->header.mode & MODE_NONBLOCK_ACCESS) {
    if (f->_reset != rs->_reset) {
      f->_lo = 0;
      f->_reset = rs->_reset;
    }

    if (f->_lo < rs->high) {
      // Read as much as possible
      uint32 len = (rs->high - f->_lo);
      if (count < len) len = count;

      for (uint32 i = 0; i < len; ++i) {
        if (NULL != read_buff) read_buff[i] = stream_buff[f->_lo];
        f->_lo++;
      }

      if (f->_lo == rs->high) {
        rs->late = rs->late - 1;
        if (rs->late == 0) {
          stream_reset(rs);
        }
      }

      err = len;
      // Signal afterwards in non blocking mode
      // We want to read in one shot and stop
      // afterwards
    } else if (f->_lo == rs->high) {
      err = EOF_ERROR;
    } else {
      panic(L"Stream reader desynced");
    }
  } else {
    uint32 i;

    for (i = 0; i < count; ++i) {

      if (f->_reset != rs->_reset) {
        f->_lo = 0;
        f->_reset = rs->_reset;
      }

      if (f->_lo == rs->high) {
        rs->late = rs->late - 1;
        if (rs->late == 0) {
          stream_reset(rs);
        }
      }

      while (f->_lo == rs->high) {
        condvar_mutexless_wait(streamcv);

        if (f->_reset != rs->_reset) {
          f->_lo = 0;
          f->_reset = rs->_reset;
        }
      }

      if (NULL != read_buff) read_buff[i] = stream_buff[f->_lo];

      f->_lo = f->_lo + 1;
    }

    if (f->_lo == rs->high) {
      rs->late = rs->late - 1;
      if (rs->late == 0) {
        stream_reset(rs);
      }
    }

    err = count;
  }
  if (inter_disabled) enable_interrupts();

  return err;
}

error_code stream_open_file(fs_header* header, native_string parts,
                            uint8 depth, file_mode mode, file** result) {
  
  error_code err = NO_ERROR;
  stream_file* strm = NULL;

  if (depth == 0) return FNF_ERROR;

  if (0 == kstrcmp(STDIN_PART, parts)) {
    strm = CAST(stream_file*, kmalloc(sizeof(stream_file)));
    if (NULL == strm) {
      err = MEM_ERROR;
    } else {
      err = new_stream_file(strm, mode, &stdin);
    }
  } else if (0 == kstrcmp(STDOUT_PART, parts)) {
    strm = CAST(stream_file*, kmalloc(sizeof(stream_file)));
    if (NULL == strm) {
      err = MEM_ERROR;
    } else {
      err = new_stream_file(strm, mode, &stdout);
    }
  } else {
    err = FNF_ERROR;
  }

  *result = CAST(file*, strm);

  return err;
}

error_code mount_streams(vfnode* parent) {
  error_code err = NO_ERROR;

  __std_stream_vtable._file_open = stream_open_file;
  // __std_stream_vtable._mkdir = stream_mkdir;
  // __std_stream_vtable._rename = stream_rename;
  __std_stream_vtable._remove = stream_remove;

  fs_std_stream.kind = STREAM;
  fs_std_stream._vtable = &__std_stream_vtable;

  __std_rw_file_stream_vtable._file_close = stream_close;
  __std_rw_file_stream_vtable._file_len = stream_len;
  __std_rw_file_stream_vtable._file_move_cursor = stream_move_cursor;
  __std_rw_file_stream_vtable._file_read = stream_read;
  __std_rw_file_stream_vtable._file_set_to_absolute_position =
      stream_set_to_absolute_position;
  __std_rw_file_stream_vtable._file_write = stream_write;

  // Init streams
  if (ERROR(err = new_raw_stream(&stdin, FALSE))) return err;
  if (ERROR(err = new_raw_stream(&stdout, FALSE))) return err;

  // Init mount point
  vfnode* dev_node = CAST(vfnode*, kmalloc(sizeof(vfnode)));
  new_vfnode(dev_node, "SYS", TYPE_MOUNTPOINT);
  dev_node->_value.mountpoint.mounted_fs = &fs_std_stream;
  vfnode_add_child(parent, dev_node);

  return err;
}

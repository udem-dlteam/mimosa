#include "drivers/filesystem/include/stdstream.h"
#include "drivers/filesystem/include/vfs.h"
#include "general.h"
#include "rtlib.h"
#include "thread.h"

#define STREAM_DEFAULT_LEN 100

static raw_stream stdin;
static raw_stream stdout;

static file_vtable __std_rw_stream_vtable;

static error_code new_raw_stream(raw_stream* rs, bool autoresize);
static error_code new_stream_file(stream_file* rs, raw_stream* source);

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
void stream_reset_cursor(file* f) {
    return;
}

static error_code stream_move_cursor(file* f, int32 n) {
    return UNIMPL_ERROR;
}

static error_code stream_set_to_absolute_position(file* f, uint32 position) {
    return UNIMPL_ERROR;
}

static size_t stream_len(file* f) {
    return 0;
}

// -------------------------------------------------------------
// Stream management
// -------------------------------------------------------------

static error_code new_raw_stream(raw_stream* rs, bool autoresize) {
    if(NULL == rs) return ARG_ERROR;

    error_code err = NO_ERROR;
    rs->len = STREAM_DEFAULT_LEN;
    rs->buff = kmalloc(sizeof(uint8) * rs->len);
    
    if(NULL == rs->buff) return MEM_ERROR;
    
    rs->autoresize = autoresize;
    rs->low = rs->high = 0;

    condvar* readycv = CAST(condvar*, kmalloc(sizeof(condvar)));

    if(NULL == readycv) {
        kfree(rs->buff);
        return MEM_ERROR;
    }

    rs->readycv = new_condvar(readycv);

    return err;
}

static error_code new_stream_file(stream_file* rs,file_mode mode, raw_stream* source) {
  error_code err = NO_ERROR;

  if (NULL == rs) {
    return ARG_ERROR;
  }

  if (NULL == source) {
    return ARG_ERROR;
  }

  rs->header.mode = mode;
  rs->header._vtable = &__std_rw_stream_vtable;
  rs->source = source;

  return err;
}

static error_code stream_close(file* ff) {
    error_code err = NO_ERROR;
    stream_file* f = CAST(stream_file*, ff);

    // TODO free all of it

    return err;
}
static error_code stream_write(file* ff, void* buff, uint32 count) {
    error_code err = NO_ERROR;
    stream_file* f = CAST(stream_file*, ff);
    raw_stream* rs = f->source;
    condvar* streamcv = rs->readycv;
    uint8* stream_buff = CAST(uint8*, rs->buff);
    uint8* source_buff = CAST(uint8*, buff);

    bool inter_disabled = ARE_INTERRUPTS_ENABLED();

    {
      if (inter_disabled) disable_interrupts();

      // Loop like this and do not use mem copy because
      // we want to signal every new character
      int i;
      for (i = 0; i < count; ++i) {
        int next_hi = (rs->high + 1) % rs->len;

        if (next_hi != rs->low) {
          stream_buff[rs->high] = source_buff[i];
          rs->high = next_hi;
          condvar_mutexless_signal(streamcv);
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
    raw_stream* rs = f->source;
    condvar* streamcv = rs->readycv;
    uint8* stream_buff = CAST(uint8*, rs->buff);
    uint8* source_buff = CAST(uint8*, buff);

    bool inter_disabled = ARE_INTERRUPTS_ENABLED();

    {
      if (inter_disabled) disable_interrupts();

      int i;
      for(i = 0; i < count; ++i) {
        while (rs->low == rs->high) {
          condvar_mutexless_wait(circular_buffer_cv);
        }

        source_buff[i] = stream_buff[rs->low];
        rs->low = (rs->low + 1) % rs->len;

        condvar_mutexless_signal(circular_buffer_cv);
      }

      if (HAS_NO_ERROR(err) && i == count) err = count;

      if (inter_disabled) enable_interrupts();
    }

    return err;
}

error_code stream_open_file(native_string path, file_mode mode, file** result) {
  error_code err = NO_ERROR;
  stream_file* strm;

  if (0 == kstrcmp(STDIN, path)) {
    strm = CAST(stream_file*, kmalloc(sizeof(stream_file)));
    if (NULL == strm) {
      err = MEM_ERROR;
    } else {
      err = new_stream_file(strm, &stdin);
    }
  } else if (0 == kstrcmp(STDOUT, path)) {
    strm = CAST(stream_file*, kmalloc(sizeof(stream_file)));
    if (NULL == strm) {
      err = MEM_ERROR;
    } else {
      err = new_stream_file(strm, &stdout);
    }
  } else {
    err = FNF_ERROR;
  }

  return err;
}

error_code init_streams() {
    error_code err = NO_ERROR;

    __std_rw_stream_vtable._file_close = stream_close;
    __std_rw_stream_vtable._file_len = stream_len;
    __std_rw_stream_vtable._file_move_cursor = stream_move_cursor;
    __std_rw_stream_vtable._file_read = stream_read;
    __std_rw_stream_vtable._file_set_to_absolute_position = stream_set_to_absolute_position;
    __std_rw_stream_vtable._file_write = stream_write;

    if(ERROR(err = new_raw_stream(&stdin, FALSE))) return err;
    if(ERROR(err = new_raw_stream(&stdout, FALSE))) return err;

    return err;
}
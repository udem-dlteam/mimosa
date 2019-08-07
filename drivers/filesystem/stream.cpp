#include "drivers/filesystem/include/stream.h"
#include "drivers/filesystem/include/vfs.h"
#include "general.h"
#include "rtlib.h"
#include "thread.h"

#define STREAM_DEFAULT_LEN 100

static raw_stream stdin;
static raw_stream stdout;

static file_vtable __stream_vtable;

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

    rwmutex* rwm = CAST(rwmutex*, kmalloc(sizeof(rwmutex)));

    if (NULL == rwm) {
      kfree(rs->buff);
      return MEM_ERROR;
    }

    condvar* readycv = CAST(condvar*, kmalloc(sizeof(condvar)));

    if(NULL == readycv) {
        kfree(rs->buff);
        kfree(rwm);
        return MEM_ERROR;
    }

    rs->readycv = new_condvar(readycv);
    rs->mut = new_rwmutex(rwm);

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
  rs->header._vtable = &__stream_vtable;
  rs->source = source;

  return err;
}

static error_code stream_close(file* ff) {
    error_code err = NO_ERROR;
    stream_file* f = CAST(stream_file*, ff);


    return err;
}
static error_code stream_write(file* ff, void* buff, uint32 count) {
    error_code err = NO_ERROR;
    stream_file* f = CAST(stream_file*, ff);
    raw_stream* rs = f->source;
    CLI();

    int next_hi = (circular_buffer_hi + 1) % BUFFER_SIZE;

    if (next_hi != circular_buffer_lo) {
      circular_buffer[circular_buffer_hi] = ch;
      circular_buffer_hi = next_hi;
      condvar_mutexless_signal(circular_buffer_cv);
    } else if(rs->autoresize) {
        //Resize
    }

    STI();

    return err;
}
static error_code stream_read(file* ff, void* buf, uint32 count) {
    error_code err = NO_ERROR;
    stream_file* f = CAST(stream_file*, ff);


    return err;
}


error_code init_streams() {
    error_code err = NO_ERROR;

    __stream_vtable._file_close = stream_close;
    __stream_vtable._file_len = stream_len;
    __stream_vtable._file_move_cursor = stream_move_cursor;
    __stream_vtable._file_read = stream_read;
    __stream_vtable._file_set_to_absolute_position = stream_set_to_absolute_position;
    __stream_vtable._file_write = stream_write;

    if(ERROR(err = new_raw_stream(&stdin, FALSE))) return err;
    if(ERROR(err = new_raw_stream(&stdout, FALSE))) return err;

    return err;
}
#ifndef __STREAM_H
#define __STREAM_H

#include "drivers/filesystem/include/vfs.h"
#include "thread.h"

typedef struct raw_stream_struct raw_stream;
typedef struct stream_file_struct stream_file;

struct raw_stream_struct {
    rwmutex* mut;
    condvar* readycv;
    size_t len;
    uint32 low;
    uint32 high;
    bool autoresize;
    void* buff;
};

struct stream_file_struct {
    file header;
    raw_stream* source;
};

error_code init_streams();

error_code stream_open_file(native_string path, file_mode mode, file** result);

#endif
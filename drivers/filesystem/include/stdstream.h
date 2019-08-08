#ifndef __STREAM_H
#define __STREAM_H

#include "vfs.h"
#include "thread.h"

typedef struct raw_stream_struct raw_stream;
typedef struct stream_file_struct stream_file;

extern native_string STDIN_PATH;
extern native_string STDOUT_PATH;

struct raw_stream_struct {
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

error_code mount_streams(vfnode* parent);

error_code stream_open_file(native_string path, file_mode mode, file** result);

#endif
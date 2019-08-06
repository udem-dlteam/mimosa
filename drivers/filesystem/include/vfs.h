#ifndef __FS_H
#define __FS_H

#include "general.h"

#define MODE_PLUS (1 << 7)
#define MODE_READ (1 << 0)
#define MODE_READ_WRITE (MODE_PLUS | MODE_READ)
#define MODE_TRUNC (1 << 1)
#define MODE_TRUNC_PLUS (MODE_PLUS | MODE_TRUNC)
#define MODE_APPEND (1 << 2)
#define MODE_APPEND_PLUS (MODE_PLUS | MODE_APPEND)

typedef enum fs_kind {
    FAT,
    UART,
    STREAM
} fs_kind;

// A file system descriptor header
typedef struct fs_header_struct {
    fs_kind kind;
} fs_header;

typedef struct file_struct file;

typedef struct file_vtable_struct {
  error_code (*_file_move_cursor)(file* f, uint32 mvmt);
  error_code (*_file_set_to_absolute_position)(file* f, uint32 position);
  error_code (*_file_close)(file* f);
  error_code (*_file_write)(file* f, void* buff, uint32 count);
  error_code (*_file_read)(file* f, void* buff, uint32 count);
} file_vtable;

// A file descriptor header
struct file_struct {
    fs_header* _fs_header;
    file_vtable* _vtable;
};

error_code init_vfs();

#define file_move_cursor(f, mvmt) f->_vtable->_file_move_cursor(f, mvmt)
#define file_set_to_absolute_position(f, position) f->_vtable->_file_set_to_absolute_position(f, position)
#define file_close(f) f->_vtable->_file_close(f)
#define file_write(f, buff, count) f->_vtable->_file_write(f,buff,count)
#define file_read(f, buff, count) f->_vtable->_file_read(f, buff, count)

error_code file_open(native_string path, native_string mode, file** result);

// void test_method() {
//     file* f;
//     uint32 dir =5;
//     file_move_cursor(f, dir);
// }

#endif
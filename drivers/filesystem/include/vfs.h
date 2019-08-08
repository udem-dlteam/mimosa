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
#define MODE_NONBLOCK_ACCESS (1 << 3)

typedef uint8 file_mode;

#define IS_MODE_NONBLOCK(md) ((md) & MODE_NONBLOCK_ACCESS)
#define IS_REGULAR_FILE(tpe) ((tpe) & TYPE_REGULAR)
#define IS_FOLDER(tpe) ((tpe) & TYPE_FOLDER)

#define NAME_MAX 1024 + 1

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
typedef struct DIR_struct DIR;
typedef struct dirent_struct dirent;

typedef struct file_vtable_struct {
  error_code (*_file_move_cursor)(file* f, int32 mvmt);
  error_code (*_file_set_to_absolute_position)(file* f, uint32 position);
  error_code (*_file_close)(file* f);
  error_code (*_file_write)(file* f, void* buff, uint32 count);
  error_code (*_file_read)(file* f, void* buff, uint32 count);
  size_t     (*_file_len)(file* f);
  dirent*    (*_readdir)(DIR* dir);
} file_vtable;

// A file descriptor header
struct file_struct {
    fs_header* _fs_header;
    file_vtable* _vtable;
    file_mode mode;
};

struct dirent_struct {
  uint8 d_type;
  native_char d_name[NAME_MAX + 1];
};

struct DIR_struct {
  dirent ent;
  file* f;
};

struct stat {
  uint8 st_mode;
  uint32 st_size;
};

error_code init_vfs();

// -----------------------------------------------------------------------------------
// Exposed methods
// -----------------------------------------------------------------------------------

#define file_move_cursor(f, mvmt) CAST(file*, f)->_vtable->_file_move_cursor(CAST(file*, f), mvmt)
#define file_set_to_absolute_position(f, position) CAST(file*, f)->_vtable->_file_set_to_absolute_position(CAST(file*, f), position)
#define file_close(f) CAST(file*, f)->_vtable->_file_close(CAST(file*, f))
#define file_write(f, buff, count) CAST(file*, f)->_vtable->_file_write(CAST(file*, f),buff,count)
#define file_read(f, buff, count) CAST(file*, f)->_vtable->_file_read(CAST(file*, f), buff, count)
#define file_len(f) (CAST(file*, f))->_vtable->_file_len(CAST(file*, f))
#define readdir(dir) (CAST(file*, dir->f))->_vtable->_readdir(dir)

error_code file_open(native_string path, native_string mode, file** result);
error_code normalize_path(native_string path, native_string new_path);
bool parse_mode(native_string mode, file_mode* result);

DIR* opendir(const char* path);
error_code closedir(DIR* dir);

#endif
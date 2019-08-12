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

#define IS_MODE_WRITE_ONLY(md) \
  (((md) == MODE_TRUNC) || (md) == ((MODE_TRUNC | MODE_NONBLOCK_ACCESS)))

#define TYPE_REGULAR (1 << 0)
#define TYPE_FOLDER (1 << 1)
#define TYPE_VIRTUAL (1 << 2)
#define TYPE_MOUNTPOINT (1 << 3)
#define TYPE_VFOLDER (TYPE_VIRTUAL | TYPE_FOLDER)
#define TYPE_VFILE (TYPE_VIRTUAL | TYPE_REGULAR)

typedef uint8 file_mode;
typedef uint8 file_type;

#define IS_MODE_NONBLOCK(md) ((md)&MODE_NONBLOCK_ACCESS)

#define IS_REGULAR_FILE(tpe) ((tpe)&TYPE_REGULAR)
#define IS_FOLDER(tpe) ((tpe)&TYPE_FOLDER)
#define IS_VIRTUAL(tpe) ((tpe)&TYPE_VIRTUAL)

#define NAME_MAX 1024 + 1

typedef enum fs_kind { FAT, UART, STREAM } fs_kind;

// A file system descriptor header

typedef struct file_struct file;
typedef struct vfnode_struct vfnode;
typedef struct DIR_struct DIR;
typedef struct VDIR_struct VDIR;
typedef struct dirent_struct dirent;
typedef struct fs_vtable_struct fs_vtable;

typedef struct fs_header_struct {
  fs_kind kind;
  fs_vtable* _vtable;
} fs_header;

typedef struct short_file_name_struct {
  native_char name[12];
} short_file_name;

typedef struct file_vtable_struct {
  error_code (*_file_move_cursor)(file* f, int32 mvmt);
  error_code (*_file_set_to_absolute_position)(file* f, uint32 position);
  error_code (*_file_close)(file* f);
  error_code (*_file_write)(file* f, void* buff, uint32 count);
  error_code (*_file_read)(file* f, void* buff, uint32 count);
  size_t (*_file_len)(file* f);
  dirent* (*_readdir)(DIR* dir);
} file_vtable;

struct fs_vtable_struct {
  error_code (*_file_open)(fs_header* header, short_file_name* parts,
                           uint8 depth, file_mode mode, file** result);
  error_code (*_mkdir)(fs_header* header, short_file_name* parts, uint8 depth,
                       file** result);
  error_code (*_rename)(fs_header* header, file* source, short_file_name* parts,
                        uint8 depth);
};

// A file descriptor header
struct file_struct {
  fs_header* _fs_header;
  file_vtable* _vtable;
  native_string path;
  file_type type;
  file_mode mode;
};

struct vfnode_struct {
  file header;
  vfnode* _first_child;
  vfnode* _next_sibling;
  vfnode* _parent;
  // vfnode* _last_sibling; // This will be required if we want to remove them
  native_string name;
  union {
    struct {
      fs_header* mounted_fs;
    } mountpoint;
    struct {
      file* linked_file;
    } file;
  } _value;
};

struct dirent_struct {
  file_type d_type;
  native_char d_name[NAME_MAX + 1];
};

struct DIR_struct {
  dirent ent;
  file* f;
};

struct VDIR_struct {
  DIR header;
  vfnode* child_cursor;
};

struct stat {
  file_mode st_mode;
  uint32 st_size;
};

error_code init_vfs();

// -----------------------------------------------------------------------------------
// Exposed methods
// -----------------------------------------------------------------------------------

void vfnode_add_child(vfnode* parent, vfnode* child);

vfnode* new_vfnode(vfnode* vf, native_string name, file_type type);

#define file_move_cursor(f, mvmt) \
  CAST(file*, f)->_vtable->_file_move_cursor(CAST(file*, f), mvmt)

#define file_set_to_absolute_position(f, position)                        \
  CAST(file*, f)->_vtable->_file_set_to_absolute_position(CAST(file*, f), \
                                                          position)

#define file_close(f) CAST(file*, f)->_vtable->_file_close(CAST(file*, f))

/**
 * error_code file_write(file* f, void* b, uint32 n)
 *
 * Write n bytes to a file f from the buffer b.
 * b cannot be null or an ARG_ERROR will be returned.
 * n can be 0.
 *
 */
#define file_write(f, buff, count) \
  CAST(file*, f)->_vtable->_file_write(CAST(file*, f), buff, count)

/**
 * error_code file_read(file* f, void* b, uint32 n)
 *
 * Read n bytes into a buffer b from the file f.
 * A NULL buffer is tolerated. The read will be done exactly
 * like a normal read operation, but no data will be copied anywhere.
 * Count may be 0.
 *
 */
#define file_read(f, buff, count) \
  CAST(file*, f)->_vtable->_file_read(CAST(file*, f), buff, count)

#define file_len(f) (CAST(file*, f))->_vtable->_file_len(CAST(file*, f))

#define readdir(dir) (CAST(file*, dir->f))->_vtable->_readdir(CAST(DIR*, dir))

#define file_is_dir(f) IS_FOLDER(((f)->type))

#define file_is_reg(f) IS_REGULAR_FILE(((f)->type))

#define fs_file_open(fs, parts, depth, mode, result) \
  CAST(fs_header*, fs)                               \
      ->_vtable->_file_open(CAST(fs_header*, fs), parts, depth, mode, result)

#define fs_mkdir(fs, parts, depth, result) \
  CAST(fs_header*, fs)                     \
      ->_vtable->_mkdir(CAST(fs_header*, fs), parts, depth, result)

#define fs_rename(fs, f, parts, depth) \
  CAST(fs_header*, fs)                 \
      ->_vtable->_rename(CAST(fs_header*, fs), CAST(file*, f), parts, depth)

error_code file_open(native_string path, native_string mode, file** result);
error_code file_rename(native_string old_name, native_string new_name);
error_code mkdir(native_string path, file** result);

error_code normalize_path(native_string path, native_string new_path);
short_file_name* decompose_path(native_string normalize_path, uint8* __count);
bool parse_mode(native_string mode, file_mode* result);

DIR* opendir(const char* path);
error_code closedir(DIR* dir);

#endif
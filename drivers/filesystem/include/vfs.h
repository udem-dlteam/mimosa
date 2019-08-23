#ifndef __FS_H
#define __FS_H

#include "general.h"

#define DIR_FILE_TYPE_UNKNOWN 0
#define DIR_FILE_TYPE_FIFO 1
#define DIR_FILE_TYPE_CHR 2
#define DIR_FILE_TYPE_DIR 4
#define DIR_FILE_TYPE_BLK 6
#define DIR_FILE_TYPE_REG 8
#define DIR_FILE_TYPE_LNK 10
#define DIR_FILE_TYPE_SOCK 12

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
typedef struct stat_buff_struct stat_buff;
typedef struct vfolder_struct vfolder;

typedef struct fs_header_struct {
  fs_kind kind;
  fs_vtable* _vtable;
} fs_header;

struct stat_buff_struct {
  fs_header* fs;
  file_type type;
  uint32 bytes;
  uint32 fs_block_size;
  uint32 last_modifs_epochs_secs;
  uint32 creation_time_epochs_secs;
};

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
  error_code (*_file_open)(fs_header* header, native_string parts,
                           uint8 depth, file_mode mode, file** result);
  error_code (*_mkdir)(fs_header* header, native_string name, uint8 depth,
                       file** result);
  error_code (*_rename)(fs_header* header, file* source, native_string name,
                        uint8 depth);
  error_code (*_remove)(fs_header* header, file* source);
  error_code (*_stat)(fs_header* header, file* source, stat_buff* buf);
};

// A file descriptor header
struct file_struct {
  fs_header* _fs_header;
  file_vtable* _vtable;
  native_string name;
  file_type type;
  file_mode mode;
};

struct vfnode_struct {
  file_type type;
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
      uint32 identifier;
      error_code (*_vf_node_open)(uint32 id, file_mode mode, file** result);
    } file_gate;
  } _value;
};

struct vfolder_struct {
  file header;
  vfnode* node;
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

#define fs_remove(fs, f) \
  CAST(fs_header*, fs)->_vtable->_remove(CAST(fs_header*, fs), CAST(file*, f))

#define fs_stat(fs, f, buf) \
  CAST(fs_header*, fs)      \
      ->_vtable->_stat(CAST(fs_header*, fs), CAST(file*, f), buf)

error_code file_open(native_string path, native_string mode, file** result);
error_code file_rename(native_string old_name, native_string new_name);
error_code file_remove(native_string path);
error_code file_stat(native_string path, stat_buff* buff);
error_code mkdir(native_string path, file** result);

error_code normalize_path(native_string old_path, native_string new_path, uint8* _depth);
bool parse_mode(native_string mode, file_mode* result);

DIR* opendir(const char* path);
error_code closedir(DIR* dir);

#endif

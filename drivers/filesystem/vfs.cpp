#include "general.h"
#include "include/fat.h"
#include "include/stdstream.h"
#include "include/vfs.h"
#include "rtlib.h"
#include "term.h"
#include "uart.h"

fs_header __vfs;
static fs_vtable __vfs_vtable;
static file_vtable __vfnode_vtable;
static vfnode sys_root;
static vfnode dev_mnt_pt;

static error_code vfnode_close(file *f);
static size_t vfnode_len(file *f);
static error_code vfnode_move_cursor(file *f, int32 mvmt);
static error_code vfnode_set_abs(file *f, uint32 pos);
static error_code vfnode_read(file *f, void *buff, uint32 count);
static error_code vfnode_write(file *f, void *buff, uint32 count);
static dirent *vfnode_readdir(DIR *dir);

static error_code vfnode_close(file *f) { return PERMISSION_ERROR; }

static size_t vfnode_len(file *f) { return 0; }

static error_code vfnode_move_cursor(file *f, int32 mvmt) {
  return PERMISSION_ERROR;
}

static error_code vfnode_set_abs(file *f, uint32 pos) {
  return PERMISSION_ERROR;
}

static error_code vfnode_write(file *f, void *buff, uint32 count) {
  return PERMISSION_ERROR;
}

static error_code vfnode_read(file *f, void *buff, uint32 count) {
  return PERMISSION_ERROR;
}

static dirent *vfnode_readdir(DIR *dir) {
  VDIR *vdir = CAST(VDIR *, dir);

  if (NULL == vdir->child_cursor) {
    return NULL;
  }

  vfnode *child = vdir->child_cursor;
  dirent *result = &dir->ent;

  switch (child->type) {
  case TYPE_FOLDER:
  case TYPE_VFOLDER:
    result->d_type = DIR_FILE_TYPE_DIR;
    break;
  case TYPE_MOUNTPOINT:
    result->d_type = DIR_FILE_TYPE_DIR;
    break;
  case TYPE_VFILE:
    result->d_type = DIR_FILE_TYPE_BLK;
    break;
  case TYPE_REGULAR:
    result->d_type = DIR_FILE_TYPE_REG;
    break;
  default:
    result->d_type = DIR_FILE_TYPE_UNKNOWN;
    break;
  }

  native_string p1 = dir->ent.d_name;
  native_string p2;

  p1 = copy_without_trailing_spaces(CAST(uint8 *, child->name), p1, 8);
  *p1++ = '.';
  p2 = p1;
  p1 = copy_without_trailing_spaces(CAST(uint8 *, child->name) + 8, p1, 3);
  if (p1 == p2)
    p1--; // erase the dot
  *p1++ = '\0';

  vdir->child_cursor = child->_next_sibling;

  return result;
}

error_code normalize_path(native_string old_path, native_string new_path,
                          uint8 *_depth) {
  // The path normalization algorithm does two passes on the text.
  // The first one is to correctly analyze the paths (and collapse levels)
  // while the second one removes the slashes to add \0 instead (except
  // for the first forward slash since it is the name of the root folder).
  // The resulting path is all the folder and file names that are explored
  // in the path as a string. The length of all the strings is passed in the
  // error code.
  uint32 i = 0, j = 0;
  uint8 depth;
  native_string path = old_path;
  new_path[i++] = '/';
  new_path[i++] = '/';

  while (path[0] != '\0') {
    if (path[0] == '/') {
      i = 2;
      path++;
    } else if (path[0] == '.' && (path[1] == '\0' || path[1] == '/')) {
      if (path[1] == '\0')
        path += 1;
      else
        path += 2;
    } else if (path[0] == '.' && path[1] == '.' &&
               (path[2] == '\0' || path[2] == '/')) {
      if (path[2] == '\0')
        path += 2;
      else
        path += 3;
      i--;
      while (i > 0 && new_path[i - 1] != '/')
        i--;
      if (i == 0)
        return FNF_ERROR;
    } else {
      while (path[0] != '\0' && path[0] != '/') {
        if (i >= NAME_MAX)
          return FNF_ERROR;
        new_path[i++] = (*path >= 'a' && *path <= 'z') ? (*path - 32) : *path;
        path++;
      }
      if (path[0] != '\0') {
        if (i >= NAME_MAX)
          return FNF_ERROR;
        new_path[i++] = '/';
        path++;
      }
    }
  }

  // If the last is not a forward slash
  // add it.
  if (new_path[i - 1] != '/') {
    new_path[i++] = '/';
  }

  new_path[i] = '\0';

  j = 1;
  depth = 0;
  while (new_path[j] != '\0') {
    if (new_path[j] == '/') {
      new_path[j] = '\0';
      ++depth;
    }
    ++j;
  }

  *_depth = depth;

  return i;
}

bool parse_mode(native_string mode, file_mode *result) {
  file_mode f_mode = 0;
  native_char *c = mode;

  // We break away from the linux "fopen" spec. The order in which the
  // mode specifiers are entered does not matter for mimosa.
  // We also have more specifiers (non blocking for instance).

  while (*c != '\0') {
    switch (*c++) {
    case 'a':
    case 'A':
      f_mode |= MODE_APPEND;
      break;
    case 'w':
    case 'W':
      f_mode |= MODE_TRUNC;
      break;
    case 'r':
    case 'R':
      f_mode |= MODE_READ;
      break;
    case '+':
      f_mode |= MODE_PLUS;
      break;
    case 'x':
    case 'X':
      f_mode |= MODE_NONBLOCK_ACCESS;
      break;
    default:
      goto vfs_parse_mode_loop_end;
      break;
    }
  }

vfs_parse_mode_loop_end:
  *result = f_mode;
  return '\0' ==
         *c; // if we stopped at the null terminator, we did not fail anywhere
}

static vfnode *explore(native_char **_parts, uint8 *_depth) {
  vfnode *last_candidate = NULL;
  vfnode *scout = &sys_root;
  native_string parts = *_parts;
  uint8 depth = *_depth;

  do {
    if (0 == kstrcmp(scout->name, parts)) {
      last_candidate = scout;
      scout = scout->_first_child;
      depth--;
      while (*parts++ != '\0')
        ;
    } else if (NULL != scout->_next_sibling) {
      scout = scout->_next_sibling;
    } else {
      scout = NULL;
    }
  } while (NULL != scout && depth > 0);

  *_depth = depth;
  *_parts = parts;
  return last_candidate;
}

error_code file_remove(native_string path) {
  error_code err = NO_ERROR;
  file *f;

  if (ERROR(err = file_open(path, "r", &f))) {
    return err;
  }

  fs_header *fs = f->_fs_header;

  error_code rmv_err = fs_remove(fs, f);

  if (ERROR(err = file_close(f))) {
    return err;
  }

  return ERROR(rmv_err) ? rmv_err : err;
}

error_code file_rename(native_string old_name, native_string new_name) {
  error_code err = NO_ERROR;
  native_char normalized_path[NAME_MAX + 1];
  uint8 depth_new;

  if (ERROR(err = normalize_path(new_name, normalized_path, &depth_new))) {
    return err;
  }

  // disable_interrupts(); // interrupts are disabled to ensure atomicity

  // Rename acts like a copy. It is more efficient to simply copy the directory
  // entry For that, we will deactivate the old entry (to allow the old folder
  // to still work) and copy the directory entry

  file *old_file = NULL;
  native_string p = normalized_path;
  vfnode *deepest = explore(&p, &depth_new);

  if (ERROR(err = file_open(old_name, "r", &old_file))) {
    goto rename_end;
  }

  if (NULL == deepest) {
    err = FNF_ERROR;
  } else if (deepest->type & TYPE_MOUNTPOINT) {
    // Make sure the FS of the mountpoint and the FS of the file
    // is the same:
    fs_header *target_fs = deepest->_value.mountpoint.mounted_fs;

    if (old_file->_fs_header != target_fs) {
      // This is an error because file_rename does not work across file systems.
      err = ARG_ERROR;
    } else {
      err = fs_rename(target_fs, old_file, p, depth_new);
    }
  } else {
    err = FNF_ERROR;
  }

rename_end:

  if (NULL != old_file) {
    file_close(old_file);
  }
  // enable_interrupts(); // interrupts are disabled to ensure atomicity
  return err;
}

error_code file_stat(native_string path, stat_buff *buf) {
  error_code err = NO_ERROR;
  file *f = NULL;

  if (ERROR(err = file_open(path, "r", &f))) {
    return err;
  }

  if (NULL == f->_fs_header) {
    // TODO: stderr would be cool here
    debug_write("Calling file_stat on a file without a FS");
  } else {
    err = fs_stat(f->_fs_header, f, buf);
  }

  if (NULL != f) {
    file_close(f);
  }

  return err;
}

error_code mkdir(native_string path, file **result) {
  uint8 depth;
  error_code err = NO_ERROR;
  file *hit;
  native_char normalized_path[NAME_MAX + 1];

  if (ERROR(err = normalize_path(path, normalized_path, &depth))) {
#ifdef SHOW_DISK_INFO
    term_write(cout, "Failed to normalize the path\n\r");
#endif
    return err;
  }

  native_string p = normalized_path;
  vfnode *deepest = explore(&p, &depth);

  if (NULL == deepest) {
    err = FNF_ERROR;
  } else if (deepest->type & TYPE_MOUNTPOINT) {
    fs_header *fs = deepest->_value.mountpoint.mounted_fs;
    err = fs_mkdir(fs, p, depth, &hit);
  } else {
    // TODO if it is a VFOLDER is might be possible to add a folder
    err = FNF_ERROR;
  }

  if (HAS_NO_ERROR(err)) {
    *result = hit;
  }

  return err;
}

error_code file_open(native_string path, native_string mode, file **result) {
  uint8 depth;
  error_code err = NO_ERROR;
  file *hit = NULL;
  file_mode md;
  native_char normalized_path[NAME_MAX + 1];

  if (ERROR(err = normalize_path(path, normalized_path, &depth))) {
    return err;
  }

  native_char *p = normalized_path;
  vfnode *deepest = explore(&p, &depth);

  if (!parse_mode(mode, &md)) {
    return ARG_ERROR;
  }

  if (NULL == deepest) {
    err = FNF_ERROR;
  } else if (depth == 0) {
    if ((deepest->type & TYPE_VFOLDER) == TYPE_VFOLDER) {
      hit = CAST(file *, kmalloc(sizeof(vfolder)));
      hit->_fs_header = &__vfs;
      hit->mode = md;
      hit->type = deepest->type;
      hit->_vtable = &__vfnode_vtable;
      uint32 len = kstrlen(p) + 1;
      hit->name = CAST(native_string, kmalloc(sizeof(native_char) * len));
      memcpy(hit->name, p, len);
      CAST(vfolder *, hit)->node = deepest;
    } else if ((deepest->type & TYPE_VFILE) == TYPE_VFILE) {
      uint32 id = deepest->_value.file_gate.identifier;
      err = deepest->_value.file_gate._vf_node_open(id, md, &hit);
    } else if ((deepest->type & TYPE_MOUNTPOINT) == TYPE_MOUNTPOINT) {
      fs_header *fs = deepest->_value.mountpoint.mounted_fs;
      err = fs_file_open(fs, p, depth, md, &hit);
    } else {
      err = FNF_ERROR;
    }
  } else {
    // depth > 0
    if (deepest->type & TYPE_MOUNTPOINT) {
      fs_header *fs = deepest->_value.mountpoint.mounted_fs;
      err = fs_file_open(fs, p, depth, md, &hit);
    } else {
      err = FNF_ERROR;
    }
  }

  if (HAS_NO_ERROR(err)) {
    *result = hit;
  }

  return err;
}

// -----------------------------------------------------------------------------------
// WIP
// -----------------------------------------------------------------------------------

DIR *opendir(const char *path) {
  file *f;
  DIR *dir;
  error_code err;

  if (ERROR(err = file_open(CAST(native_string, path), "r", &f))) {
    return NULL;
  }

  if (!IS_FOLDER(f->type)) {
    file_close(f); // ignore error
    return NULL;
  }

  if (IS_VIRTUAL(f->type)) {
    if ((dir = CAST(DIR *, kmalloc(sizeof(VDIR)))) == NULL) {
      return NULL;
    }
    vfnode *vf = CAST(vfolder *, f)->node;
    VDIR *vdir = CAST(VDIR *, dir);
    vdir->child_cursor = vf->_first_child;
  } else {
    if ((dir = CAST(DIR *, kmalloc(sizeof(DIR)))) == NULL) {
      return NULL;
    }
  }

  dir->f = f;

  return dir;
}

error_code closedir(DIR *dir) {
  file_close(dir->f); // ignore error
  kfree(dir);         // ignore error

  return NO_ERROR;
}

void inline set_dir_entry_size(FAT_directory_entry *de, uint32 sz) {
  for (int i = 0; i < 4; ++i) {
    de->DIR_FileSize[i] = as_uint8(sz, i);
  }
}

void vfnode_add_child(vfnode *parent, vfnode *child) {
  if (NULL == parent->_first_child) {
    parent->_first_child = child;
  } else {
    vfnode *prev, *scout;
    scout = parent->_first_child;

    do {
      prev = scout;
      scout = scout->_next_sibling;
    } while (NULL != scout);

    prev->_next_sibling = child;
    // child->prev_sibling = prev;
  }

  child->_parent = parent;
}

vfnode *new_vfnode(vfnode *vf, native_string name, file_type type) {
  if (NULL == name) {
    return NULL;
  }

  uint32 name_len = kstrlen(name) + 1; // count the zero
  vf->type = type;
  if (NULL == (vf->name = CAST(native_string,
                               kmalloc(sizeof(native_char) * name_len)))) {
    return NULL;
  }

  memcpy(vf->name, name, name_len);
  vf->_first_child = vf->_next_sibling = vf->_parent = NULL;

  return vf;
}

error_code vfs_open(fs_header *header, native_string parts, uint8 depth,
                    file_mode mode, file **result) {
  return UNKNOWN_ERROR;
}

error_code vfs_mkdir(fs_header *header, native_string name, uint8 depth,
                     file **result) {
  return UNKNOWN_ERROR;
}

error_code vfs_rename(fs_header *header, file *source, native_string name,
                      uint8 depth) {
  return UNKNOWN_ERROR;
}

error_code vfs_remove(fs_header *header, file *source) { return UNKNOWN_ERROR; }

error_code vfs_stat(fs_header *header, file *source, stat_buff *buf) {
  error_code err = NO_ERROR;

  buf->bytes = file_len(source);
  buf->fs = header;
  buf->fs_block_size = 0;
  buf->creation_time_epochs_secs = 0;
  buf->last_modifs_epochs_secs = 0;
  buf->type = source->type;

  return err;
}

error_code init_vfs() {
  error_code err = NO_ERROR;

  __vfs_vtable._file_open = vfs_open;
  __vfs_vtable._mkdir = vfs_mkdir;
  __vfs_vtable._remove = vfs_remove;
  __vfs_vtable._rename = vfs_rename;
  __vfs_vtable._stat = vfs_stat;

  __vfs._vtable = &__vfs_vtable;
  __vfs.kind = NONE;

  __vfnode_vtable._file_close = vfnode_close;
  __vfnode_vtable._file_len = vfnode_len;
  __vfnode_vtable._file_move_cursor = vfnode_move_cursor;
  __vfnode_vtable._file_set_to_absolute_position = vfnode_set_abs;
  __vfnode_vtable._file_read = vfnode_read;
  __vfnode_vtable._file_write = vfnode_write;
  __vfnode_vtable._readdir = vfnode_readdir;

  new_vfnode(&sys_root, "/", TYPE_VFOLDER);
  new_vfnode(&dev_mnt_pt, "DEV", TYPE_VFOLDER);

  vfnode_add_child(&sys_root, &dev_mnt_pt);

  if (ERROR(err = mount_streams(&sys_root))) {
    return err;
  }

  if (ERROR(err = mount_fat(&sys_root))) {
    return err;
  }

  return err;
}

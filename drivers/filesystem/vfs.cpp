#include "include/vfs.h"
#include "include/stdstream.h"
#include "general.h"
#include "rtlib.h"
#include "include/fat.h"
#include "term.h"

static file_vtable __vfnode_vtable;
static vfnode sys_root;

static error_code vfnode_close(file* f);
static size_t vfnode_len(file* f);
static error_code vfnode_move_cursor(file* f, int32 mvmt);
static error_code vfnode_set_abs(file* f, uint32 pos);
static error_code vfnode_read(file* f, void* buff, uint32 count);
static error_code vfnode_write(file* f, void* buff, uint32 count);
static dirent*    vfnode_readdir(DIR* dir);

// error_code stat(native_string path, struct stat* buf) {
//   file* f;
//   error_code err;

//   if (ERROR(err = open_file(path, "r", &f))) return err;

//   buf->st_mode = f->mode;
//   buf->st_size = f->length;

//   return close_file(f);
// }

static error_code vfnode_close(file* f) {
  return PERMISSION_ERROR;
}

static size_t vfnode_len(file* f) {
  return 0;
}

static error_code vfnode_move_cursor(file* f, int32 mvmt) {
  return PERMISSION_ERROR;
}

static error_code vfnode_set_abs(file* f, uint32 pos) {
  return PERMISSION_ERROR;
}

static error_code vfnode_write(file* f, void* buff, uint32 count) {
  return PERMISSION_ERROR;
}

static error_code vfnode_read(file* f, void* buff, uint32 count) {
  return PERMISSION_ERROR;
}

static dirent*    vfnode_readdir(DIR* dir) {
  return NULL;
}

error_code normalize_path(native_string path, native_string new_path) {
  uint32 i = 0;

  new_path[i++] = '/';

  while (path[0] != '\0') {
    if (path[0] == '/') {
      i = 1;
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
      while (i > 0 && new_path[i - 1] != '/') i--;
      if (i == 0) return FNF_ERROR;
    } else {
      while (path[0] != '\0' && path[0] != '/') {
        if (i >= NAME_MAX) return FNF_ERROR;
        new_path[i++] = (*path >= 'a' && *path <= 'z') ? (*path - 32) : *path;
        path++;
      }
      if (path[0] != '\0') {
        if (i >= NAME_MAX) return FNF_ERROR;
        new_path[i++] = '/';
        path++;
      }
    }
  }

  new_path[i] = '\0';

  return NO_ERROR;
}

short_file_name* decompose_path(native_string normalized_Path, uint8* __count) {
  *__count = 0;
  uint8 count = 0;
  uint8 entry = 0;
  short_file_name* result = NULL;
  native_char* scout = normalized_Path;

  if('\0' == *scout) return NULL;
  if('/' != *scout) return NULL;
  
  do {
    count += (*scout == '/');
  } while(*scout++ != '\0');
  count++;

  if(NULL == (result = CAST(short_file_name*, kmalloc(count * sizeof(short_file_name_struct))))) {
    return NULL; // meh
  }

  char* p = normalized_Path;
  debug_write(normalized_Path);
  while (entry < count) {
    int i = 0;
    bool seen_next_slash = FALSE;
    
    if (*p == '\0') {
      // Overshot the count
      --count;
      goto decompose_path_end;
    }

    while (*p != '\0' && *p != '.') {
      if (*p == '/') {
        seen_next_slash = TRUE;
        break;
      }
      if (i < 8) {
        result[entry].name[i] = *p;
      }
      i++;
      p++;
    }

    while (i < 8) result[entry].name[i++] = ' ';

    i = 0;

    if (*p == '.') {
      p++;
      while (*p != '\0') {

        if(*p == '/') {
          seen_next_slash = TRUE;
          break;
        }

        if (i < 3) result[entry].name[8 + i] = *p;
        i++;
        p++;
      }
    }

    while (i < 3) result[entry].name[8 + i++] = ' ';

    if (!seen_next_slash) {
      while (*p != '/') {
        p++;
      }
    }

    p++;
    entry++;
  }
decompose_path_end:
  *__count = count;

  // for (int i = 0; i < count; ++i) {
  //   term_writeline(cout);
  //   for (int j = 0; j < FAT_NAME_LENGTH; ++j) {
  //     term_write(cout, CAST(native_char, result[i].name[j]));
  //   }
  //   term_writeline(cout);
  // }
  return result;
}

bool parse_mode(native_string mode, file_mode* result) {
  file_mode f_mode = 0;
  native_char* c = mode;

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
  return '\0' == *c; // if we stopped at the null terminator, we did not fail anywhere
}

static vfnode* explore(short_file_name* parts, uint8 *depth) {
  vfnode* last_candidate = NULL;
  vfnode* scout = &sys_root;
  uint8 bottom = *depth;
  do {
    if(0 == kstrcmp(scout->name, parts[bottom - *depth].name)) {
      last_candidate = scout;
      scout = scout->_first_child;
      *depth = *depth - 1;
    } else if(NULL != scout->_next_sibling) {
      scout = scout->_next_sibling;
    } else {
      scout = NULL;
    }
  } while(NULL != scout);

  return last_candidate;
}

error_code file_open(native_string path, native_string mode, file** result) {
  error_code err = NO_ERROR;
  native_char normalized_path[NAME_MAX + 1];
  uint8 depth;

  if (ERROR(err = normalize_path(path, normalized_path))) {
#ifdef SHOW_DISK_INFO
    term_write(cout, "Failed to normalize the path\n\r");
#endif
    return err;
  }
  debug_write(normalized_path);
  short_file_name* parts = decompose_path(normalized_path, &depth);

  term_write(cout, "Parts:");
  for(int i = 0; i < depth; ++i) {
    term_write(cout, "|");
    for(int j = 0; j < 11; ++j) {
      term_write(cout, parts[i].name[j]);
    }
    term_write(cout, "|");
  }
  term_writeline(cout);

  vfnode* deepest = explore(parts, &depth);
  
  if(depth > 0) {
    debug_write("Depth left to explore");

    file_mode md;
    file* hit = NULL;

    if (!parse_mode(mode, &md)) {
      return ARG_ERROR;
    }

    if (ERROR(err = stream_open_file(path, md, &hit)) && FNF_ERROR != err) {
      return err;
    }

    if (HAS_NO_ERROR(err)) goto vfs_open_found;

    if (ERROR(err = fat_open_file(path, md, &hit)) && FNF_ERROR != err) {
      return err;
    }

    if (HAS_NO_ERROR(err)) goto vfs_open_found;

    // More search

  vfs_open_found:
    if (HAS_NO_ERROR(err)) {
      *result = hit;
    }
  } else {
    debug_write("Went all in");
  }

  return err;
}

// -----------------------------------------------------------------------------------
// WIP
// -----------------------------------------------------------------------------------


DIR* opendir(const char* path) {
  file* f;
  DIR* dir;
  error_code err;

  if (ERROR(err = file_open(CAST(native_string, path),"r", &f))) {
    debug_write("Error while opening the file :/");
    return NULL;
  }

  if (!IS_FOLDER(f->type) || (dir = CAST(DIR*, kmalloc(sizeof(DIR)))) == NULL) {
    debug_write("Failed to open the DIR!");
    file_close(f);  // ignore error
    return NULL;
  }

  dir->f = f;

  return dir;
}

error_code closedir(DIR* dir) {
  file_close(dir->f);  // ignore error
  kfree(dir);          // ignore error

  return NO_ERROR;
}

void inline set_dir_entry_size(FAT_directory_entry* de, uint32 sz) {
  for (int i = 0; i < 4; ++i) {
    de->DIR_FileSize[i] = as_uint8(sz, i);
  }
}

error_code init_vfs() {
  error_code err = NO_ERROR;

  __vfnode_vtable._file_close = vfnode_close;
  __vfnode_vtable._file_len = vfnode_len;
  __vfnode_vtable._file_move_cursor = vfnode_move_cursor;
  __vfnode_vtable._file_set_to_absolute_position = vfnode_set_abs;
  __vfnode_vtable._file_read = vfnode_read;
  __vfnode_vtable._file_write = vfnode_write;
  __vfnode_vtable._readdir = vfnode_readdir;

  {
    sys_root.header.mode = MODE_READ;
    sys_root.header.type = TYPE_VFOLDER;
    sys_root.header._fs_header = NULL;
    sys_root.header._vtable = &__vfnode_vtable;
    sys_root.name = "           ";
    sys_root._first_child = sys_root._next_sibling = sys_root._parent = NULL;
  }

  if(ERROR(err = init_streams())) {
    return err;
  }

  if(ERROR(err = init_fat())) {
    return err;
  }

  return err;
}
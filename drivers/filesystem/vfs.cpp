#include "include/vfs.h"
#include "include/stdstream.h"
#include "general.h"
#include "rtlib.h"
#include "include/fat.h"
#include "term.h"

error_code init_vfs() {
  error_code err = NO_ERROR;
  
  if(ERROR(err = init_streams())) {
    return err;
  }

  if(ERROR(err = init_fat())) {
    return err;
  }

  return err;
}

// error_code stat(native_string path, struct stat* buf) {
//   file* f;
//   error_code err;

//   if (ERROR(err = open_file(path, "r", &f))) return err;

//   buf->st_mode = f->mode;
//   buf->st_size = f->length;

//   return close_file(f);
// }

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

bool parse_mode(native_string mode, file_mode* result) {
  bool success = TRUE;
  file_mode f_mode = 0;
  native_char first = mode[0];

  if (success = ('\0' != first)) {
    switch (first) {
      case 'r':
      case 'R':
        f_mode = MODE_READ;
        break;
      case 'w':
      case 'W':
        f_mode = MODE_TRUNC;
        break;
      case 'a':
      case 'A':
        f_mode = MODE_APPEND;
        break;
      default:
        success = FALSE;
        return success;
        break;
    }

    native_char snd = mode[1];
    if ('+' == snd) {
      f_mode |= MODE_PLUS;
    }
  }

  *result = f_mode;
  return success;
}

error_code file_open(native_string path, native_string mode, file** result) {
  error_code err = NO_ERROR;
  file_mode md;
  file* hit = NULL;

  if (!parse_mode(mode, &md)) {
    return ARG_ERROR;
  }

  // TODO: cascade with other file systems
  if (ERROR(err = fat_open_file(path, md, &hit)) && FNF_ERROR != err) {
    return err;
  }

  if (NULL != hit) {
    *result = hit;
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

  if (!S_ISDIR(f->mode) || (dir = CAST(DIR*, kmalloc(sizeof(DIR)))) == NULL) {
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
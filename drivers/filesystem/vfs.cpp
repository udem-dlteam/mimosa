#include "include/vfs.h"
#include "include/fat.h"
#include "general.h"
#include "term.h"

error_code init_vfs() {
    debug_write("Init VFS");
    init_fat();


    return 0;
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

bool parse_mode(native_string mode, file_mode* result) {
  bool success = TRUE;
  file_mode f_mode = 0;
  native_char first = mode[0];

  if(success = ('\0' != first)) {

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
    if('+' == snd) {
      f_mode |= MODE_PLUS;
    }
  }

  *result = f_mode;
  return success;
}

error_code file_open(native_string path, native_string mode, file** result) {
    return 0;
}

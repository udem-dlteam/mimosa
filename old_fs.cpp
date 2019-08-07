
// file: "fs.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Nov 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "fs.h"
#include "disk.h"
#include "fat32.h"
#include "ide.h"
#include "rtlib.h"
#include "term.h"

//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------

// FAT file system implementation.



error_code _file_set_pos_from_start(file* f, uint32 position);






//-----------------------------------------------------------------------------

DIR* opendir(const char* path) {
  file* f;
  DIR* dir;
  error_code err;

  if (ERROR(err = open_file(CAST(native_string, path),"r", &f))) {
    debug_write("Error while opening the file :/");
    return NULL;
  }

  if (!S_ISDIR(f->mode) || (dir = CAST(DIR*, kmalloc(sizeof(DIR)))) == NULL) {
    close_file(f);  // ignore error
    return NULL;
  }

  dir->f = f;

  return dir;
}

static native_string copy_without_trailing_spaces(uint8* src, native_string dst,
                                                  uint32 n) {
  uint32 i;
  uint32 end = 0;

  for (i = 0; i < n; i++) {
    dst[i] = src[i];
    if (src[i] != ' ') end = i + 1;
  }

  return dst + end;
}

struct dirent* readdir(DIR* dir) {
  file* f = dir->f;
  error_code err;

  switch (f->fs->kind) {
    case FAT12_FS:
    case FAT16_FS:
    case FAT32_FS: {
      FAT_directory_entry de;

      while ((err = read_file(f, &de, sizeof(de))) == sizeof(de)) {
        if (de.DIR_Name[0] == 0) break;
        if (de.DIR_Name[0] != 0xe5) {
          if ((de.DIR_Attr & FAT_ATTR_HIDDEN) == 0 &&
              (de.DIR_Attr & FAT_ATTR_VOLUME_ID) == 0) {
            native_string p1 = dir->ent.d_name;
            native_string p2;

            p1 = copy_without_trailing_spaces(&de.DIR_Name[0], p1, 8);
            *p1++ = '.';
            p2 = p1;
            p1 = copy_without_trailing_spaces(&de.DIR_Name[8], p1, 3);
            if (p1 == p2) p1--;
            *p1++ = '\0';

            dir->ent.d_type = (de.DIR_Attr & FAT_ATTR_DIRECTORY)
                                  ? S_IFDIR
                                  : (de.DIR_Attr ? 0 : S_IFREG);

            return &dir->ent;
          }
        }
      }

      return NULL;
    }
  }

  return NULL;
}

error_code closedir(DIR* dir) {
  close_file(dir->f);  // ignore error
  kfree(dir);          // ignore error

  return NO_ERROR;
}





void inline set_dir_entry_size(FAT_directory_entry* de, uint32 sz) {
  for (int i = 0; i < 4; ++i) {
    de->DIR_FileSize[i] = as_uint8(sz, i);
  }
}

//-----------------------------------------------------------------------------

error_code stat(native_string path, struct stat* buf) {
  file* f;
  error_code err;

  if (ERROR(err = open_file(path, "r", &f))) return err;

  buf->st_mode = f->mode;
  buf->st_size = f->length;

  return close_file(f);
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //

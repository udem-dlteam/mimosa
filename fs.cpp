
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

#define MAX_NB_MOUNTED_FS 8

typedef struct fs_module_struct {
  file_system* mounted_fs[MAX_NB_MOUNTED_FS];
  uint32 nb_mounted_fs;
} fs_module;

static fs_module fs_mod;

//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------

// FAT file system implementation.

error_code __attribute__((optimize("O0")))
open_root_dir(file_system* fs, file** result) {
  file* f = CAST(file*, kmalloc(sizeof(file)));

  if (f == NULL) return MEM_ERROR;

  switch (fs->kind) {
    case FAT12_FS:
    case FAT16_FS: {
#ifdef SHOW_DISK_INFO
      term_write(cout, "Opening FAT12/FAT16\n\r");
#endif
      f->fs = fs;
      f->current_cluster = 1;  // so that EOC is detected at end of dir
      f->current_section_start = fs->_.FAT121632.first_data_sector -
                                 fs->_.FAT121632.root_directory_sectors;
      f->current_section_length = fs->_.FAT121632.root_directory_sectors
                                  << fs->_.FAT121632.log2_bps;
      f->current_section_pos = 0;
      f->current_pos = 0;
      f->length = f->current_section_length;
      f->mode = S_IFDIR;
      break;
    }

    case FAT32_FS: {
      error_code err;
      if (ERROR(err = fat_32_open_root_dir(fs, f))) {
        return err;
      }
      break;
    }

    default:
      kfree(f);
      return UNIMPL_ERROR;
  }

  *result = f;

  return NO_ERROR;
}

error_code open_root_dir_at_file_entry(file* f, file** result) {
  error_code err = NO_ERROR;

  file* root_dir = CAST(file*, kmalloc(sizeof(file)));

  if (NULL == root_dir) {
    err = MEM_ERROR;
  } else {
    root_dir->fs = f->fs;
    root_dir->first_cluster = 2; // TODO: not necessairly true
    root_dir->current_cluster = f->entry.cluster;
    root_dir->current_section_start = f->entry.section_start;
    root_dir->current_section_length = f->entry.section_length;
    root_dir->current_section_pos = f->entry.section_pos;
    root_dir->current_pos = f->entry.current_pos;
    root_dir->length = 0;  // Length for directories is not important
    root_dir->mode = S_IFDIR;

    *result = root_dir;
  }

  return err;
}

static error_code normalize_path(native_string path, native_string new_path) {
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
        new_path[i++] = *path++;
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

error_code open_file(native_string path, file** result) {
  file_system* fs;
  file* f;

  if (fs_mod.nb_mounted_fs == 0) {
    return FNF_ERROR;
  }

  fs = fs_mod.mounted_fs[0];

  switch (fs->kind) {
    case FAT12_FS:
    case FAT16_FS:
    case FAT32_FS: {
      FAT_directory_entry de;
      error_code err;
      native_char normalized_path[NAME_MAX + 1];
      native_string p = normalized_path;

      if (ERROR(err = normalize_path(path, normalized_path))) {
#ifdef SHOW_DISK_INFO
        term_write(cout, "Failed to normalize the path\n\r");
#endif
        return err;
      }

      if (ERROR(err = open_root_dir(fs, &f))) {
#ifdef SHOW_DISK_INFO
        term_write(cout, "Error loading the root dir: ");
        term_write(cout, err);
        term_writeline(cout);
#endif
        return err;
      }

#ifdef SHOW_DISK_INFO
      term_write(cout, "\n\rOpened the root dir...");
      term_writeline(cout);
      term_write(cout, "Normalized name: '");
      term_write(cout, normalized_path);
      term_write(cout, "'\n\r");
#endif

      for (;;) {
        uint8 name[FAT_NAME_LENGTH];
        uint32 i;

        if (*p != '\0' && (*p++ != '/' || !S_ISDIR(f->mode))) {
          close_file(f);  // ignore error
          return FNF_ERROR;
        }

        if (*p == '\0') break;

        i = 0;

        while (*p != '\0' && *p != '.' && *p != '/') {
          if (i < 8) name[i] = *p;
          i++;
          p++;
        }

        while (i < 8) name[i++] = ' ';

        i = 0;

        if (*p == '.') {
          p++;
          while (*p != '\0' && *p != '/') {
            if (i < 3) name[8 + i] = *p;
            i++;
            p++;
          }
        }

        while (i < 3) name[8 + i++] = ' ';

        file entry_file = *f;
        while ((err = read_file(f, &de, sizeof(de))) == sizeof(de)) {
          if (de.DIR_Name[0] == 0) break;  // No more entries
          // Only verify the file if it's readable
          if (de.DIR_Name[0] != 0xe5 && (de.DIR_Attr & FAT_ATTR_HIDDEN) == 0 &&
              (de.DIR_Attr & FAT_ATTR_VOLUME_ID) == 0) {
            // Compare the names
            for (i = 0; i < FAT_NAME_LENGTH; i++) {
              if (de.DIR_Name[i] != name[i]) break;
            }

            if (i == FAT_NAME_LENGTH) {
              // All the characters have been compared successfuly
              if (ERROR(err = close_file(f))) return err;
              f = CAST(file*, kmalloc(sizeof(file)));
              if (f == NULL) return MEM_ERROR;
              f->fs = fs;
              f->current_cluster =
                  (CAST(uint32, as_uint16(de.DIR_FstClusHI)) << 16) +
                  as_uint16(de.DIR_FstClusLO);
              f->first_cluster = f->current_cluster;
              f->current_section_start =
                  fs->_.FAT121632.first_data_sector +
                  ((f->current_cluster - 2) << fs->_.FAT121632.log2_spc);
              f->current_section_length =
                  1 << (fs->_.FAT121632.log2_bps + fs->_.FAT121632.log2_spc);
              f->current_section_pos = 0;
              f->current_pos = 0;
              f->length = as_uint32(de.DIR_FileSize);

              if (de.DIR_Attr & FAT_ATTR_DIRECTORY) {
                f->mode = S_IFDIR;
              } else {
                f->mode = S_IFREG;
              }

              // Setup the entry file
              f->entry.cluster = entry_file.current_cluster;
              f->entry.current_pos = entry_file.current_pos;
              f->entry.section_length = entry_file.current_section_length;
              f->entry.section_pos = entry_file.current_section_pos;
              f->entry.section_start = entry_file.current_section_start;

              goto found;
            } else {
              // Update the entry and position values
              entry_file = *f;
            }
          }
        }

        close_file(f);  // ignore error

        if (ERROR(err)) return err;

        return FNF_ERROR;

      found:;
      }

      break;
    }

    default:
      return UNIMPL_ERROR;
  }

  *result = f;

  return NO_ERROR;
}

error_code close_file(file* f) {
  kfree(f);
  return NO_ERROR;
}

static error_code find_first_empty_FAT_cluster(file_system* fs,
                                               uint32* cluster) {
  if (fs->kind != FAT32_FS) {
    return UNIMPL_ERROR;
  } else {
    return fat_32_find_first_empty_cluster(fs, cluster);
  }
}

static error_code get_fat_link_value(file_system* fs, uint32 cluster,
                                     uint32* value) {
  if (fs->kind != FAT32_FS) {
    return UNIMPL_ERROR;
  } else {
    return fat_32_get_fat_link_value(fs, cluster, value);
  }
}

static error_code set_fat_link_value(file_system* fs, uint32 cluster,
                                     uint32 value) {
  if (fs->kind != FAT32_FS) {
    return UNIMPL_ERROR;
  } else {
    return fat_32_set_fat_link_value(fs, cluster, value);
  }
}

static error_code next_FAT_section(file* f) {
  file_system* fs = f->fs;
  uint32 n = f->current_cluster;
  uint32 offset;
  uint32 sector_pos;
  uint32 cluster;
  cache_block* cb;
  error_code err;

  if (fs->kind == FAT12_FS) {
    debug_write("Reading the next FAT12 section");
    offset = n + (n >> 1);

    sector_pos =
        fs->_.FAT121632.reserved_sectors + (offset >> fs->_.FAT121632.log2_bps);

    offset &= ~(~0U << fs->_.FAT121632.log2_bps);

    if (ERROR(err =
                  disk_cache_block_acquire(fs->_.FAT121632.d, sector_pos, &cb)))
      return err;

    cluster = *CAST(uint8*, cb->buf + offset);

    if (offset == ~(~0U << fs->_.FAT121632.log2_bps)) {
      if (ERROR(err = disk_cache_block_release(cb))) return err;
      if (ERROR(err = disk_cache_block_acquire(fs->_.FAT121632.d,
                                               sector_pos + 1, &cb)))
        return err;
      offset = 0;
    } else
      offset++;

    if (n & 1)
      cluster =
          (cluster >> 4) + (CAST(uint32, *CAST(uint8*, cb->buf + offset)) << 4);
    else
      cluster =
          cluster + (CAST(uint32, *CAST(uint8*, cb->buf + offset) & 0xf) << 8);

    if (ERROR(err = disk_cache_block_release(cb))) return err;

    if (cluster >= 0xff8) return EOF_ERROR;
  } else {
    if (fs->kind == FAT16_FS) {
      offset = n << 1;
    } else {
      offset = n << 2;
    }

    sector_pos =
        fs->_.FAT121632.reserved_sectors + (offset >> fs->_.FAT121632.log2_bps);

    offset &= ~(~0U << fs->_.FAT121632.log2_bps);

    if (ERROR(err = disk_cache_block_acquire(fs->_.FAT121632.d, sector_pos,
                                             &cb))) {
      return err;
    }

    if (fs->kind == FAT16_FS) {
      cluster = *CAST(uint16*, cb->buf + offset);
      if (ERROR(err = disk_cache_block_release(cb))) return err;
      if (cluster >= 0xfff8) return EOF_ERROR;
    } else {
      cluster = *CAST(uint32*, cb->buf + offset) & 0x0fffffff;

      if (ERROR(err = disk_cache_block_release(cb))) {
        return err;
      }

      if (cluster >= FAT_32_EOF) {
        return EOF_ERROR;
      }
    }
  }

  f->current_cluster = cluster;
  f->current_section_length =
      1 << (fs->_.FAT121632.log2_bps + fs->_.FAT121632.log2_spc);
  f->current_section_start = ((cluster - 2) << fs->_.FAT121632.log2_spc) +
                             fs->_.FAT121632.first_data_sector;

  f->current_section_pos = 0;

  return NO_ERROR;
}

error_code write_file(file* f, void* buff, uint32 count, bool auto_flush) {
  error_code err = NO_ERROR;

  if (count < 1) return err;

  if (f->wrt.buff_sz == 0) {
    // No buffer is currently allocated for the file.
    // We allocate it with twice the count to amortize
    // the cost
    uint32 desired_size = count << 1;
    f->wrt.len = 0;
    f->wrt.buff = (uint8*)kmalloc(sizeof(uint8) * desired_size);

    if (NULL == f->wrt.buff) {
      // Not enough memory to allocate this big of a buffer.
      err = MEM_ERROR;
      return err;
    }

    f->wrt.buff_sz = desired_size;
  }

  if (f->wrt.len + count > f->wrt.buff_sz) {
    // This buffer is too small, so we need to reallocate it.
    // Again, take twice the size. However, since count might be big, we double
    // until counts fits in.
    uint8* old_buff = f->wrt.buff;
    uint32 old_len = f->wrt.len;
    uint32 old_sz = f->wrt.buff_sz;
    uint32 total_sz;

    do {
      f->wrt.buff_sz = f->wrt.buff_sz << 1;  // Double it
    } while (f->wrt.len + count > f->wrt.buff_sz);

    uint8* new_buff = CAST(uint8*, kmalloc(sizeof(uint8) * f->wrt.buff_sz));

    if (NULL == new_buff) {
      // We could not allocate enough memory for the write buffer
      err = MEM_ERROR;
      return err;
    }

    memcpy(new_buff, old_buff, old_len);
    kfree(old_buff);

    f->wrt.buff = new_buff;
    f->wrt.len = old_len;
    // f->wrt.buff_sz = Already correct
  }

#ifdef CHECK_ASSERTIONS
  if (f->wrt.len + count > f->wrt.buff_sz)
    fatal_error("The new buffer is of incorrect size");
#endif

  uint8* file_buff = f->wrt.buff;
  uint32 len = f->wrt.len;

  memcpy(file_buff + len, buff, count);

  // Update the len
  f->wrt.len = len + count;

  if (auto_flush) {
    err = flush_file(f);
  } else {
    err = count;
  }

  return err;
}

error_code read_file(file* f, void* buf, uint32 count) {
  if (count > 0) {
    file_system* fs = f->fs;
    error_code err;

    switch (fs->kind) {
      case FAT12_FS:
      case FAT16_FS:
      case FAT32_FS: {
        uint32 n;
        uint8* p;

        if (!S_ISDIR(f->mode)) {
          uint32 left = f->length - f->current_pos;
          if (count > left) count = left;
        }

        n = count;
        p = CAST(uint8*, buf);

        while (n > 0) {
          uint32 left1;
          uint32 left2;

          if (f->current_section_pos >= f->current_section_length) {
            if (ERROR(err = next_FAT_section(f))) {
              if (err != EOF_ERROR) return err;
              break;
            }
          }

          left1 = f->current_section_length - f->current_section_pos;

          if (left1 > n) left1 = n;

          while (left1 > 0) {
            cache_block* cb;
            if (ERROR(err = disk_cache_block_acquire(
                          fs->_.FAT121632.d,
                          f->current_section_start +
                              (f->current_section_pos >> DISK_LOG2_BLOCK_SIZE),
                          &cb))) {
              return err;
            }

            left2 = (1 << DISK_LOG2_BLOCK_SIZE) -
                    (f->current_section_pos & ~(~0U << DISK_LOG2_BLOCK_SIZE));

            if (left2 > left1) left2 = left1;

            memcpy(p,
                   cb->buf + (f->current_section_pos &
                              ~(~0U << DISK_LOG2_BLOCK_SIZE)),
                   left2);

            if (ERROR(err = disk_cache_block_release(cb))) return err;

            left1 -= left2;
            f->current_section_pos += left2;
            f->current_pos += left2;
            n -= left2;
            p += left2;  // We read chars, skip to the next part
          }
        }

        return count - n;
      }

      default:
        return UNIMPL_ERROR;
    }
  }

  return 0;
}

error_code __attribute__((optimize("O0")))
create_file(native_string path, file** result) {
  error_code err;
  file_system* fs;
  file* f;

  if (fs_mod.nb_mounted_fs == 0) {
    return FNF_ERROR;
  }

  fs = fs_mod.mounted_fs[0];
  switch (fs->kind) {
    case FAT12_FS:
    case FAT16_FS:
      fatal_error("Not supported");
      break;
    case FAT32_FS:
      err = fat_32_create_empty_file(fs, "TESTTTT", "TXT", result);
      break;
  }

  return err;
}

static error_code mount_FAT121632(disk* d, file_system** result) {
  file_system* fs;
  BIOS_Parameter_Block* p;
  bool expecting_FAT32;
  uint16 bps;
  uint8 log2_bps;
  uint16 spc;
  uint8 log2_spc;
  uint16 rec;
  uint16 total_sectors16;
  uint32 total_sectors;
  uint32 FAT_size;
  uint32 reserved_sectors;
  uint32 root_directory_sectors;
  uint32 first_data_sector;
  uint32 total_data_sectors;
  uint32 total_data_clusters;
  uint8 kind;
  cache_block* cb;
  error_code err;

  if (ERROR(err = disk_cache_block_acquire(d, 0, &cb))) return err;

  p = CAST(BIOS_Parameter_Block*, cb->buf);

  bps = as_uint16(p->BPB_BytsPerSec);
  log2_bps = log2(bps);

  spc = p->BPB_SecPerClus;
  log2_spc = log2(spc);

  rec = as_uint16(p->BPB_RootEntCnt);
  total_sectors16 = as_uint16(p->BPB_TotSec16);
  total_sectors = as_uint32(p->BPB_TotSec32);
  FAT_size = as_uint16(p->BPB_FATSz16);
  reserved_sectors = as_uint16(p->BPB_RsvdSecCnt);

  expecting_FAT32 = (FAT_size == 0);

#ifdef SHOW_DISK_INFO
  term_write(cout, "Expected FAT32 = ");
  term_write(cout, expecting_FAT32);
  term_writeline(cout);
#endif

  if ((1 << log2_bps) != bps || log2_bps < 9 || log2_bps > 12) {
    term_write(cout,
               "bytes per sector is not a power of 2 between 512 and 4096: ");
    term_write(cout, bps);
    term_writeline(cout);
    err = UNKNOWN_ERROR;
  } else if (d->log2_sector_size != log2_bps) {
    term_write(cout, "BytsPerSec and disk's sector size are inconsistent\n");
    err = UNKNOWN_ERROR;
  } else if ((1 << log2_spc) != spc || log2_spc > 8) {
    term_write(cout,
               "sectors per cluster is not a power of 2 between 1 and 128\n");
    err = UNKNOWN_ERROR;
  } else if (expecting_FAT32) {
    if (rec != 0) {
      term_write(cout, "RootEntCnt is not 0\n");
      err = UNKNOWN_ERROR;
    } else if (total_sectors16 != 0) {
      term_write(cout, "TotSec16 is not 0\n");
      err = UNKNOWN_ERROR;
    } else if (total_sectors == 0) {
      term_write(cout, "TotSec32 is 0\n");
      err = UNKNOWN_ERROR;
    } else {
      FAT_size = as_uint32(p->_.FAT32.BPB_FATSz32);

      if (FAT_size == 0) {
        term_write(cout, "FATSz32 is 0\n");
        err = UNKNOWN_ERROR;
      }
    }
  } else {
    if (rec == 0) {
      term_write(cout, "RootEntCnt is 0\n");
      err = UNKNOWN_ERROR;
    } else if (total_sectors == 0) {
      if (total_sectors16 == 0) {
        term_write(cout, "TotSec16 and TotSec32 are 0\n");
        err = UNKNOWN_ERROR;
      } else
        total_sectors = total_sectors16;
    } else {
      if (total_sectors16 != 0 && total_sectors16 != total_sectors) {
        term_write(cout, "TotSec16 != TotSec32\n");
        err = UNKNOWN_ERROR;
      }
    }
  }

  if (!ERROR(err)) {
    root_directory_sectors =
        (CAST(uint32, rec) * FAT_DIR_ENTRY_SIZE + (1 << log2_bps) - 1) >>
        log2_bps;

    first_data_sector = CAST(uint32, p->BPB_NumFATs) * FAT_size +
                        as_uint16(p->BPB_RsvdSecCnt) + root_directory_sectors;

    total_data_sectors = total_sectors - first_data_sector;

    total_data_clusters = total_data_sectors >> log2_spc;

#ifdef SHOW_DISK_INFO
    term_write(cout, "Total data cluster is: ");
    term_write(cout, total_data_clusters);
    term_writeline(cout);
#endif

    if (total_data_clusters == 0) {
      if (expecting_FAT32) {
        term_write(cout, "volume is FAT12 or FAT16 but FATSz16 is 0\n");
        err = UNKNOWN_ERROR;
      } else if (total_data_clusters < 4085) {
        if (d->partition_type != 1) {
          term_write(cout, "partition type is not 1\n");
          err = UNKNOWN_ERROR;
        } else
          kind = FAT12_FS;
      } else {
        if (d->partition_type != 4 && d->partition_type != 6) {
          term_write(cout, "partition type is not 4 or 6\n");
          err = UNKNOWN_ERROR;
        } else {
          kind = FAT16_FS;
        }
      }
    } else {
      if (!expecting_FAT32) {
        term_write(cout, "volume is FAT32 but FATSz16 is not 0\n");
        err = UNKNOWN_ERROR;
      } else if (d->partition_type != 11 && d->partition_type != 12) {
        term_write(cout, "partition type is not 11 nor 12\n");
        err = UNKNOWN_ERROR;
      } else {
        kind = FAT32_FS;
      }
    }
  }

  if (ERROR(err)) {
    disk_cache_block_release(cb);  // ignore error
  } else if (!ERROR(err = disk_cache_block_release(cb))) {
    fs = CAST(file_system*, kmalloc(sizeof(file_system)));
    if (fs == NULL) {
      err = MEM_ERROR;
    } else {
      fs->kind = kind;
      fs->_.FAT121632.d = d;
      fs->_.FAT121632.log2_bps = log2_bps;
      fs->_.FAT121632.log2_spc = log2_spc;
      fs->_.FAT121632.total_sectors = total_sectors;
      fs->_.FAT121632.reserved_sectors = reserved_sectors;
      fs->_.FAT121632.root_directory_sectors = root_directory_sectors;
      fs->_.FAT121632.first_data_sector = first_data_sector;
      fs->_.FAT121632.total_data_clusters = total_data_clusters;

      *result = fs;
    }
  }

  return err;
}

static error_code mount_partition(disk* d) {
#ifdef SHOW_DISK_INFO
  term_write(cout, "IN MOUNT PART\n\r");
#endif

  file_system* fs = NULL;
  error_code err;

  switch (d->partition_type) {
    case 1:     // Primary DOS 12-bit FAT
    case 4:     // Primary DOS 16-bit FAT
    case 6:     // Primary big DOS >32Mb
    case 0x0C:  // FAT32 LBA

      if (ERROR(err = mount_FAT121632(d, &fs))) {
        term_write(cout, "Failed to mount\n\r");
        return err;
      }

      break;
    default:
      term_write(cout, "Unknown partition type: ");
      term_write(cout, d->partition_type);
      term_write(cout, "\n\r");
      break;
  }

  if (fs != NULL) {
    if (fs_mod.nb_mounted_fs < MAX_NB_MOUNTED_FS) {
      fs_mod.mounted_fs[fs_mod.nb_mounted_fs++] = fs;

      term_write(cout, "Mouting partition ");

      disk_print_id(d);

      term_write(cout, " as ");

      const char* kind;

      switch (fs->kind) {
        case FAT12_FS:
          kind = "FAT12";
          break;
        case FAT16_FS:
          kind = "FAT16";
          break;
        case FAT32_FS:
          kind = "FAT32";
          break;
      }

      term_write(term_write(cout, kind), "\n");

      return NO_ERROR;
    }
  }

  return UNKNOWN_ERROR;
}

static void mount_all_partitions() {
  uint32 index;
  disk* d;

  fs_mod.nb_mounted_fs = 0;

  index = 0;

  while ((d = disk_find(index)) != NULL) {
    mount_partition(d);
    index++;
  }
}

//-----------------------------------------------------------------------------

DIR* opendir(native_string path) {
  file* f;
  DIR* dir;
  error_code err;

  if (ERROR(err = open_file(path, &f))) {
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

error_code __attribute__((optimize("O0"))) flush_file(file* f) {
  error_code err = NO_ERROR;
  uint32 count = f->wrt.len;
  file_system* fs = f->fs;
  ide_device* dev = fs->_.FAT121632.d->_.ide.dev;
  cache_block* cb;

  if (count < 1) return err;

  switch (fs->kind) {
    case FAT32_FS: {
      uint32 n;
      uint8* p;

      n = count;
      p = CAST(uint8*, f->wrt.buff);

      while (n > 0) {
        debug_write("n:"); debug_write(n);
        uint32 left1;
        uint32 left2;

        if (f->current_section_pos >= f->current_section_length) {
          if (ERROR(err = next_FAT_section(f))) {
            if (err != EOF_ERROR) {
              return err;
            } else {
              // Writing a file should not OEF, we allocate more
              uint32 cluster;
              uint32 value;

              if (ERROR(err = fat_32_find_first_empty_cluster(fs, &cluster))) {
                return err;
              }
              
              // We set the current last cluster to point towards the new one
              if (ERROR(err = fat_32_set_fat_link_value(fs,
                                                        f->current_cluster,
                                                        cluster))) {
                return err;
              }

              if (ERROR(err = fat_32_set_fat_link_value(fs, cluster,
                                                        FAT_32_EOF))) {
                return err;
              }

              // Retry to fetch the next cluster
              if (ERROR(err = next_FAT_section(f))) {
                if (err == EOF_ERROR) {
                  fatal_error(
                      "Failed to allocate a new FAT cluster, but no error was "
                      "returned");
                } else {
                  return err;
                }
              }
            }
          }
        }

        left1 = f->current_section_length - f->current_section_pos;

        if (left1 > n) left1 = n;
        while (left1 > 0) {
          cache_block* cb;
          if (ERROR(err = disk_cache_block_acquire(
                        fs->_.FAT121632.d,
                        f->current_section_start +
                            (f->current_section_pos >> DISK_LOG2_BLOCK_SIZE),
                        &cb))) {
            return err;
          }

          left2 = (1 << DISK_LOG2_BLOCK_SIZE) -
                  (f->current_section_pos & ~(~0U << DISK_LOG2_BLOCK_SIZE));

          if (left2 > left1) left2 = left1;

          uint8* sector_buffer = cb->buf + (f->current_section_pos &
                                            ~(~0U << DISK_LOG2_BLOCK_SIZE));
          // Update the cache_block buffer
          memcpy(sector_buffer, p, left2);

          if (ERROR(err = disk_cache_block_release(cb))) return err;

          // Write to disk
          if (ERROR(err = ide_write(dev, f->current_section_start,
                                    f->current_section_pos, left2,
                                    sector_buffer))) {
            return err;
          }

          left1 -= left2;
          f->current_section_pos += left2;
          f->current_pos += left2;
          n -= left2;
          p += left2;  // We read chars, skip to the next part
        }
      }

      err = count - n;
    } break;
    default:
      err = UNIMPL_ERROR;
  }

  if (!ERROR(err)) {
    if (!S_ISDIR(f->mode)) {
      // Update the directory entry
      // to set the correct length of the file
      FAT_directory_entry de;
      file entry_file_cp;
      file* entry_file;
      uint32 fz;

      if (ERROR(err = open_root_dir_at_file_entry(f, &entry_file))) {
        goto flush_file_update_dir_err_occured;
      }

      // Avoid reallocating to rewrite the root dir entry
      entry_file_cp = *entry_file;

      if (ERROR(err = read_file(entry_file, &de, sizeof(de)))) {
        goto flush_file_update_dir_err_occured;
      }

      fz = as_uint32(de.DIR_FileSize) + count;
      for (int i = 0; i < 4; ++i) {
        de.DIR_FileSize[i] = as_uint8(fz, i);
      }

      if (ERROR(err = write_file(&entry_file_cp, &de, sizeof(de), TRUE))) {
        goto flush_file_update_dir_err_occured;
      }

    flush_file_update_dir_err_occured:
      close_file(entry_file);
    }

    // Free the buffer
    kfree(f->wrt.buff);
    f->wrt.len = f->wrt.buff_sz = 0;
    f->wrt.buff = NULL;
  }

  return err;
}

void file_reset_cursor(file* f) {
  file_system* fs = f->fs;
  f->current_cluster = f->first_cluster;
  f->current_section_length =
      1 << (fs->_.FAT121632.log2_bps + fs->_.FAT121632.log2_spc);
  f->current_section_start =
      ((f->first_cluster - 2) << fs->_.FAT121632.log2_spc) +
      fs->_.FAT121632.first_data_sector;
  f->current_section_pos = 0;
  f->current_pos = 0;
}

error_code _file_set_pos_from_start(file* f, uint32 position) {
  error_code err = NO_ERROR;
  file_system* fs = f->fs;
  BIOS_Parameter_Block* p;
  cache_block* cb;
  disk* d = fs->_.FAT121632.d;

  if (position > f->length) {
    return UNKNOWN_ERROR;  // TODO: better than this
  }
  // FAT32 only
  if (HAS_NO_ERROR(err = disk_cache_block_acquire(d, 0, &cb))) {
    p = CAST(BIOS_Parameter_Block*, cb->buf);
    uint16 bytes_per_sector = as_uint16(p->BPB_BytsPerSec);

    uint32 cluster_sz = bytes_per_sector * p->BPB_SecPerClus;
    uint32 no_of_clusters =
        position /
        cluster_sz;  // determines how many cluster links we have to jump
    uint32 bytes_left_cluster = position % cluster_sz;

    file_reset_cursor(f);
    // We are now at the beginning of the file.
    // We want to go to the position wanted, so
    // we walk through the FAT chain until we read
    // as many clusters as required to get a correct position.
    for (int i = 0; i < no_of_clusters; ++i) {
      if (ERROR(err = next_FAT_section(f))) return err;
    }
    f->current_section_pos += bytes_left_cluster;
    if (ERROR(err = disk_cache_block_release(cb))) return err;

    f->current_pos = position;
  }

  return err;
}

error_code file_move_cursor(file* f, int32 n) {
  uint32 displacement;
  BIOS_Parameter_Block* p;
  cache_block* cb;
  error_code err = NO_ERROR;
  file_system* fs = f->fs;
  disk* d = fs->_.FAT121632.d;

  if(n == 0) {
    // No move
    return err;
  } 
    
  if(n < 0) {
    // Backwards move
    displacement = -n;
    bool crosses_section_boundary = displacement > f->current_section_pos; 

    debug_write("Mouvement crosses section boundary:");
    
    if(crosses_section_boundary) {
      debug_write("TRUE");
    } else {
      debug_write("FALSE");
    }

    // If we cross a section boundary, the design of the FAT FS requires to start
    // from the beginning.
    if (crosses_section_boundary) {
      return _file_set_pos_from_start(f, f->current_pos - displacement);
    } else {
      // Simply update the position
      f->current_pos -= displacement;
      f->current_section_pos -= displacement;
    }
  } else {
    // Forward move
    displacement = n;
    bool crosses_section_boundary = ((displacement + f->current_section_pos) >= f->current_section_length);

    debug_write("Mouvement crosses section boundary:");
    
    if(crosses_section_boundary) {
      debug_write("TRUE");
    } else {
      debug_write("FALSE");
    }

    if(crosses_section_boundary) {
      // We are moving forward.
      if (HAS_NO_ERROR(err = disk_cache_block_acquire(d, 0, &cb))) {
        p = CAST(BIOS_Parameter_Block*, cb->buf);
        uint16 bytes_per_sector = as_uint16(p->BPB_BytsPerSec);
        uint32 cluster_sz = bytes_per_sector * p->BPB_SecPerClus;
        
        uint32 current_section_pos = f->current_section_pos;
        uint32 section_relative_pos = (current_section_pos + displacement) % cluster_sz;

        uint32 no_of_clusters;
        if(displacement == cluster_sz) {
          return no_of_clusters = 1;
        } else {
          no_of_clusters = (displacement / cluster_sz) + 1;
        }

        for (int i = 0; i < no_of_clusters; ++i) {
          if(ERROR(err = next_FAT_section(f))) {
            break;
          }
        }

        if(HAS_NO_ERROR(err)) {
          f->current_section_pos = section_relative_pos;
          f->current_pos += displacement;
        } else {
          // Try anyways
          disk_cache_block_release(cb);
        }
      }
    } else {
      // Simply update the position
      f->current_pos += displacement;
      f->current_section_pos += displacement;
    }
  }

  return err;
}

error_code file_set_to_absolute_position(file* f, uint32 position) {
  // To goal of this method is to set the file cursor to an absolute position,
  // but maybe by using relative movements. It decides what is more appropriate.
  int32 bytes = position - f->current_pos;
  return file_move_cursor(f, bytes);
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

  if (ERROR(err = open_file(path, &f))) return err;

  buf->st_mode = f->mode;
  buf->st_size = f->length;

  return close_file(f);
}

//-----------------------------------------------------------------------------

void setup_fs() {
  disk_add_all_partitions();
  mount_all_partitions();
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //

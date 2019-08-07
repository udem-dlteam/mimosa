#include "general.h"
#include "rtlib.h"
#include "ide.h"
#include "disk.h"
#include "fat32.h"
#include "thread.h"
#include "include/vfs.h"
#include "include/fat.h"

// -------------------------------------------------------------
// Declarations for the mounted FS systems
// -------------------------------------------------------------

#define MAX_NB_MOUNTED_FS 8

typedef struct short_file_name_struct {
  native_char name[FAT_NAME_LENGTH];
} short_file_name;

typedef struct fs_module_struct {
  fat_file_system* mounted_fs[MAX_NB_MOUNTED_FS];
  uint32 nb_mounted_fs;
} fs_module;

static fs_module fs_mod;
static file_vtable _fat_vtable;

error_code new_fat_file(fat_file** result) {
  error_code err = NO_ERROR;
  fat_file* allocated = CAST(fat_file*, kmalloc(sizeof(fat_file)));

  if(NULL == allocated) {
    err = MEM_ERROR;
  } else {
    allocated->header._vtable = &_fat_vtable;
  }

  *result = allocated;

  return err;
}

static void fat_reset_cursor(file* f);
static error_code fat_move_cursor(file* f, int32 n);
static error_code fat_set_to_absolute_position(file* f, uint32 position);
static error_code fat_close_file(file* f);
static error_code fat_write_file(file* f, void* buff, uint32 count);
static error_code fat_read_file(file* f, void* buf, uint32 count);
static error_code fat_open_root_dir(fat_file_system* fs, file** result);
static error_code fat_32_find_first_empty_cluster(fat_file_system* fs,
                                                  uint32* result);
static error_code fat_32_set_fat_link_value(fat_file_system* fs, uint32 cluster,
                                            uint32 value);
static error_code fat_update_file_length(fat_file* f);
static size_t fat_file_len(file* f);

// -------------------------------------------------------------
// Mounting routines
// -------------------------------------------------------------

static error_code mount_FAT121632(disk* d, fat_file_system** result) {
  fat_file_system* fs;
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
  error_code err, release_err;

  {
    if (ERROR(err = disk_cache_block_acquire(d, 0, &cb))) return err;
    rwmutex_readlock(cb->mut);

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

    rwmutex_readunlock(cb->mut);
    release_err = disk_cache_block_release(cb);
  }

  if(ERROR(err)) return err;
  else if(ERROR(release_err)) return release_err;

  fs = CAST(fat_file_system*, kmalloc(sizeof(fat_file_system)));

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

  return err;
}

static error_code mount_partition(disk* d) {
#ifdef SHOW_DISK_INFO
  term_write(cout, "IN MOUNT PART\n\r");
#endif

  fat_file_system* fs = NULL;
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


// ------------------------------------------------------
// FAT manipulation routines
// ------------------------------------------------------

error_code fat_close_file(file* ff) {
  fat_file* f = CAST(fat_file*, ff);
  kfree(f);
  return NO_ERROR;
}

static void fat_reset_cursor(fat_file* f) {
  fat_file_system* fs = f->fs;
  f->current_cluster = f->first_cluster;
  f->current_section_length =
      1 << (fs->_.FAT121632.log2_bps + fs->_.FAT121632.log2_spc);
  f->current_section_start =
      ((f->first_cluster - 2) << fs->_.FAT121632.log2_spc) +
      fs->_.FAT121632.first_data_sector;
  f->current_section_pos = 0;
  f->current_pos = 0;
}

static error_code next_FAT_section(fat_file* f) {
  fat_file_system* fs = f->fs;
  uint32 n = f->current_cluster;
  uint32 offset;
  uint32 sector_pos;
  uint32 cluster;
  cache_block* cb;
  error_code err;

  if (fs->kind == FAT12_FS) {
    //debug_write("Reading the next FAT12 section");
    offset = n + (n >> 1);

    sector_pos =
        fs->_.FAT121632.reserved_sectors + (offset >> fs->_.FAT121632.log2_bps);

    offset &= ~(~0U << fs->_.FAT121632.log2_bps);

    {
      if (ERROR(err = disk_cache_block_acquire(fs->_.FAT121632.d, sector_pos,
                                               &cb)))
        return err;
      rwmutex_readlock(cb->mut);

      cluster = *CAST(uint8*, cb->buf + offset);

      if (offset == ~(~0U << fs->_.FAT121632.log2_bps)) {
        rwmutex_readunlock(cb->mut);
        if (ERROR(err = disk_cache_block_release(cb))) return err;
        if (ERROR(err = disk_cache_block_acquire(fs->_.FAT121632.d,
                                                 sector_pos + 1, &cb))) {
          return err;
        }
        rwmutex_readlock(cb->mut);

        offset = 0;
      } else {
        offset++;
      }

      if (n & 1) {
        cluster = (cluster >> 4) +
                  (CAST(uint32, *CAST(uint8*, cb->buf + offset)) << 4);
      } else {
        cluster = cluster +
                  (CAST(uint32, *CAST(uint8*, cb->buf + offset) & 0xf) << 8);
      }

      rwmutex_readunlock(cb->mut);
      if (ERROR(err = disk_cache_block_release(cb))) return err;
    }

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

    {
      if (ERROR(err = disk_cache_block_acquire(fs->_.FAT121632.d, sector_pos,
                                               &cb))) {
        return err;
      }

      {
        rwmutex_readlock(cb->mut);

        if (fs->kind == FAT16_FS) {
          cluster = *CAST(uint16*, cb->buf + offset);
          if (cluster >= 0xfff8) err = EOF_ERROR;
        } else {
          cluster = *CAST(uint32*, cb->buf + offset) & 0x0fffffff;
          if (cluster >= FAT_32_EOF) err = EOF_ERROR;
        }

        rwmutex_readunlock(cb->mut);
      }

      error_code release_err = disk_cache_block_release(cb);

      if (ERROR(err)) {
        return err;
      } else if (ERROR(release_err)) {
        return release_err;
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

static error_code fat_file_set_pos_from_start(fat_file* f, uint32 position) {
  error_code err, release_error = NO_ERROR;
  fat_file_system* fs = f->fs;
  BIOS_Parameter_Block* p;
  cache_block* cb;
  disk* d = fs->_.FAT121632.d;

  if (S_ISREG(f->header.mode) && (position > f->length)) {
    return UNKNOWN_ERROR;  // TODO: better than this
  }

  // FAT32 only
  if (HAS_NO_ERROR(err = disk_cache_block_acquire(d, 0, &cb))) {
    rwmutex_readlock(cb->mut);

    p = CAST(BIOS_Parameter_Block*, cb->buf);
    uint16 bytes_per_sector = as_uint16(p->BPB_BytsPerSec);
    
    uint32 cluster_sz = bytes_per_sector * p->BPB_SecPerClus;
    uint32 no_of_clusters =
        position /
        cluster_sz;  // determines how many cluster links we have to jump
    uint32 bytes_left_cluster = position % cluster_sz;

    fat_reset_cursor(f);
    // We are now at the beginning of the file.
    // We want to go to the position wanted, so
    // we walk through the FAT chain until we read
    // as many clusters as required to get a correct position.
    for (int i = 0; i < no_of_clusters; ++i) {
      if (ERROR(err = next_FAT_section(f))) {
        break;
      }
    }

    f->current_section_pos += bytes_left_cluster;
    f->current_pos = position;

    rwmutex_readunlock(cb->mut);
    release_error = disk_cache_block_release(cb);
    
    if(ERROR(err)) return err;
    if(ERROR(release_error)) return release_error;
  }

  return err;
}

static error_code fat_move_cursor(file* ff, int32 n) {
  fat_file* f = CAST(fat_file*, ff);
  uint32 displacement;
  BIOS_Parameter_Block* p;
  cache_block* cb;
  error_code err = NO_ERROR;
  fat_file_system* fs = f->fs;
  disk* d = fs->_.FAT121632.d;

  if(n == 0) {
    // No mvmt
    return err;
  } 
    
  if(n < 0) {
    // Backwards mvmt
    displacement = -n;
    bool crosses_section_boundary = displacement > f->current_section_pos;

    // If we cross a section boundary, the design of the FAT FS requires to start
    // from the beginning.
    if (crosses_section_boundary) {
      // term_write(cout, "Pos. targeted:");
      // term_write(cout, f->current_pos - displacement);
      // term_writeline(cout);
      return fat_file_set_pos_from_start(f, f->current_pos - displacement);
    } else {
      // Simply update the position
      f->current_pos -= displacement;
      f->current_section_pos -= displacement;
    }
  } else {
    // Forward mvmt
    displacement = n;
    bool crosses_section_boundary = ((displacement + f->current_section_pos) >= f->current_section_length);

    // term_write(cout, "Moving "); term_write(cout, n); term_write(cout, " positions forward");
    // term_writeline(cout);

    if(crosses_section_boundary) {
      // We are moving forward.
      if (HAS_NO_ERROR(err = disk_cache_block_acquire(d, 0, &cb))) {
        uint32 cluster_sz;

        {
          rwmutex_readlock(cb->mut);
          p = CAST(BIOS_Parameter_Block*, cb->buf);
          uint16 bytes_per_sector = as_uint16(p->BPB_BytsPerSec);
          cluster_sz = bytes_per_sector * p->BPB_SecPerClus;
          rwmutex_readunlock(cb->mut);
          if (ERROR(err = disk_cache_block_release(cb))) return err;
        }

        uint32 current_section_pos = f->current_section_pos;
        uint32 new_section_pos = current_section_pos + displacement;
        uint32 no_of_clusters = (new_section_pos / cluster_sz);
        new_section_pos %= cluster_sz; // We put back in "section length" units

        // term_write(cout, "This requires moving "); term_write(cout, no_of_clusters); term_write(cout, " cluster(s) forward");
        // term_writeline(cout);

        for (int i = 0; i < no_of_clusters; ++i) {
          if(ERROR(err = next_FAT_section(f))) {
            break;
          }
        }

        if(HAS_NO_ERROR(err)) {
          f->current_section_pos = new_section_pos;
          f->current_pos += displacement;
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

static error_code fat_set_to_absolute_position(file* ff, uint32 position) {
  fat_file* f = CAST(fat_file*, ff);
  // To goal of this method is to set the file cursor to an absolute position,
  // but maybe by using relative movements. It decides what is more appropriate.

  if(f->current_pos == position) {
    return NO_ERROR;
  }

  if (0 == position) {
    fat_reset_cursor(f);
    return NO_ERROR;
  }

  int32 mvmt = position - f->current_pos;
  return fat_move_cursor(CAST(file*, f), mvmt);
}

static short_file_name* decompose_path(native_string normalize_path, uint8* __count) {
  *__count = 0;
  uint8 count = 0;
  uint8 entry = 0;
  short_file_name* result = NULL;
  native_char* scout = normalize_path;

  if('\0' == *scout) return NULL;
  if('/' != *scout) return NULL;
  
  do {
    count += (*scout == '/');
  } while(*scout++ != '\0');

  if(NULL == (result = CAST(short_file_name*, kmalloc(count * sizeof(short_file_name_struct))))) {
    return NULL; // meh
  }

  char* p = normalize_path + 1; // skip the first '/'
  //The first entry is the root dir, skip that

  while (entry < count) {
    int i = 0;
    bool seen_next_slash = FALSE;
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

error_code fat_write_file(file* ff, void* buff, uint32 count) {
  fat_file* f = CAST(fat_file*, ff);
  error_code err = NO_ERROR;
  fat_file_system* fs = f->fs;

  if (count < 1) return err;

  switch (fs->kind) {
    case FAT32_FS: {
      uint32 n;
      uint8* p;

      n = count;
      p = CAST(uint8*, buff);

      while (n > 0) {
        uint32 left1;
        uint32 left2;

        if (f->current_section_pos >= f->current_section_length) {
          if (ERROR(err = next_FAT_section(f))) {
            if (err != EOF_ERROR) {
              return err;
            } else {
              // Writing a file should not OEF, we allocate more
              uint32 cluster;

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
                  panic(
                      L"Failed to allocate a new FAT cluster, but no error was "
                      L"returned");
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

          {
            if (ERROR(err = disk_cache_block_acquire(
                          fs->_.FAT121632.d,
                          f->current_section_start +
                              (f->current_section_pos >> DISK_LOG2_BLOCK_SIZE),
                          &cb))) {
              return err;
            }

            // Lock the access to this cache block
            rwmutex_writelock(cb->mut);

            left2 = (1 << DISK_LOG2_BLOCK_SIZE) -
                    (f->current_section_pos & ~(~0U << DISK_LOG2_BLOCK_SIZE));

            if (left2 > left1) left2 = left1;

            uint8* sector_buffer = cb->buf + (f->current_section_pos &
                                              ~(~0U << DISK_LOG2_BLOCK_SIZE));
            // Update the cache_block buffer
            memcpy(sector_buffer, p, left2);

            cb->dirty = TRUE;
            rwmutex_writeunlock(cb->mut);

            if (ERROR(err = disk_cache_block_release(cb))) return err;
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
    default: {
      debug_write("FAT FS not supported...");
      err = UNIMPL_ERROR;
    }
    break;
  }

  if (!ERROR(err) && !S_ISDIR(f->header.mode)) {
    f->length += count;
    err = fat_update_file_length(f);
  }

  return err;
}

extern volatile bool want_to_read;

error_code fat_read_file(file* ff, void* buf, uint32 count) {

  if(want_to_read) {
  debug_write("Reading FAT file");

  }

  fat_file* f = CAST(fat_file*, ff);
  if (count > 0) {
    fat_file_system* fs = f->fs;
    error_code err;

    switch (fs->kind) {
      case FAT12_FS:
      case FAT16_FS:
      case FAT32_FS: {
        uint32 n;
        uint8* p;

        if (!S_ISDIR(f->header.mode)) {
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
            
            {
              if (ERROR(
                      err = disk_cache_block_acquire(
                          fs->_.FAT121632.d,
                          f->current_section_start +
                              (f->current_section_pos >> DISK_LOG2_BLOCK_SIZE),
                          &cb))) {
                return err;
              }

              {
                rwmutex_readlock(cb->mut);

                left2 =
                    (1 << DISK_LOG2_BLOCK_SIZE) -
                    (f->current_section_pos & ~(~0U << DISK_LOG2_BLOCK_SIZE));

                if (left2 > left1) left2 = left1;

                memcpy(p,
                       cb->buf + (f->current_section_pos &
                                  ~(~0U << DISK_LOG2_BLOCK_SIZE)),
                       left2);

                rwmutex_readunlock(cb->mut);
              }

              if (ERROR(err = disk_cache_block_release(cb))) return err;
            }

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

static error_code fat_fetch_entry(fat_file_system* fs, fat_file* parent,
                                  native_char* name, fat_file** result) {
  int count = 0;
  int found_count = 0;
  error_code err = NO_ERROR;
  FAT_directory_entry de;
  fat_file* f;

#ifdef SHOW_DISK_INFO
  term_write(cout, "\n\rOpened the root dir...");
  term_writeline(cout);
  term_write(cout, "Normalized name: '");
  term_write(cout, normalized_path);
  term_write(cout, "'\n\r");
#endif

  uint32 i;

  if (!S_ISDIR(parent->header.mode)) {
    return UNKNOWN_ERROR;
  }

  uint32 cluster = parent->first_cluster;
  uint32 position = parent->current_pos;
  while ((err = fat_read_file(CAST(file*, parent), &de, sizeof(de))) == sizeof(de)) {
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
        if(ERROR(err = new_fat_file(&f))) return err;

        f->fs = fs;
        f->first_cluster = f->current_cluster =
            (CAST(uint32, as_uint16(de.DIR_FstClusHI)) << 16) +
            as_uint16(de.DIR_FstClusLO);
        f->current_section_start =
            fs->_.FAT121632.first_data_sector +
            ((f->current_cluster - 2) << fs->_.FAT121632.log2_spc);
        f->current_section_length =
            1 << (fs->_.FAT121632.log2_bps + fs->_.FAT121632.log2_spc);
        f->current_section_pos = 0;
        f->current_pos = 0;
        f->length = as_uint32(de.DIR_FileSize);

        if (de.DIR_Attr & FAT_ATTR_DIRECTORY) {
          f->header.mode = S_IFDIR;
        } else {
          f->header.mode = S_IFREG;
        }

        // Setup the entry file. It is relative to the file's
        // directory
        f->parent.first_cluster = cluster;
        f->entry.position = position;

        goto found;
      }
    }
    // Copy over the position informations
    position = f->current_pos;
  }

  // We did not find the file
  if (ERROR(err)) return err;
  return FNF_ERROR;

found:
  *result = f;
  return NO_ERROR;
}

error_code fat_32_open_root_dir(fat_file_system* fs, fat_file* f) {
#ifdef SHOW_DISK_INFO
  term_write(cout, "Loading FAT32 root dir\n\r");
#endif
  cache_block* cb;
  disk* d = fs->_.FAT121632.d;
  error_code err;
  uint16 bytes_per_sector;
  uint32 root_cluster;
  uint8 sectors_per_clusters;

  {
    if (ERROR(err = disk_cache_block_acquire(d, 0, &cb))) return err;
    rwmutex_readlock(cb->mut);

    BIOS_Parameter_Block* p = CAST(BIOS_Parameter_Block*, cb->buf);
    root_cluster = as_uint32(p->_.FAT32.BPB_RootClus);
    bytes_per_sector = as_uint16(p->BPB_BytsPerSec);
    sectors_per_clusters = p->BPB_SecPerClus;

    rwmutex_readunlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
  }

  // debug_write("In open root dir, the FS kind is: ");
  // debug_write(fs->kind);

  f->fs = fs;
  f->first_cluster = f->current_cluster = root_cluster;

#ifdef SHOW_DISK_INFO
  term_write(cout, "Root cluster is: ");
  term_write(cout, root_cluster);
  term_writeline(cout);
#endif

  f->current_section_start = fs->_.FAT121632.first_data_sector;

  // Length is not there
  f->current_section_length = bytes_per_sector * sectors_per_clusters;

#ifdef SHOW_DISK_INFO
  term_write(cout, "FAT32 ROOT DIR [sector] start=");
  term_write(cout, fs->_.FAT121632.first_data_sector);
#endif
  f->current_section_pos = 0;
  f->current_pos = 0;
  // Since the FAT32 root directory has no fixed size, we don't specify a length
  // (it would be slow to calculate it everytime...). On a directory, the length
  // is not used anyways when reading the file. We simply read until EOF.
  f->length = 0;
  f->header.mode = S_IFDIR;

  return NO_ERROR;
}

static error_code fat_open_root_dir(fat_file_system* fs, file** result) {
  error_code err;
  fat_file* f;
  
  if(ERROR(err = new_fat_file(&f))) return err;

  if (f == NULL) return MEM_ERROR;

  switch (fs->kind) {
    case FAT12_FS:
    case FAT16_FS: {
      debug_write("Opening a FAT12 root dir");
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
      f->header.mode = S_IFDIR;
      break;
    }

    case FAT32_FS: {
      if (ERROR(err = fat_32_open_root_dir(fs, f))) {
        return err;
      }
      break;
    }

    default:
      kfree(f);
      return UNIMPL_ERROR;
  }

  *result = CAST(file*, f);

  return NO_ERROR;
}

error_code fat_32_get_fat_link_value(fat_file_system* fs, uint32 cluster,
                                     uint32* value) {
  uint32 lba;
  disk* d = fs->_.FAT121632.d;
  error_code err;
  cache_block* cb;
  // entries per sector = Bytes Per sector / 4 (an entry is 4 bytes)
  uint16 entries_per_sector = (1 << fs->_.FAT121632.log2_bps) >> 2;

  if (cluster < 2) {
    panic(L"Cannot inspect lower than the second cluster entry");
  }


  { // Cache block section
    if (ERROR(err = disk_cache_block_acquire(d, 0, &cb))) return err;
    rwmutex_readlock(cb->mut);

    BIOS_Parameter_Block* p = CAST(BIOS_Parameter_Block*, cb->buf);

    lba = (cluster / entries_per_sector) + as_uint16(p->BPB_RsvdSecCnt);

    rwmutex_readunlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
  }

  uint32 offset = cluster % entries_per_sector;

  {
    if (ERROR(err = disk_cache_block_acquire(d, lba, &cb))) return err;
    rwmutex_readlock(cb->mut);

    *value = *(CAST(uint32*, cb->buf) + offset);

    rwmutex_readunlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
  }

  return err;
}

static error_code fat_32_find_first_empty_cluster(fat_file_system* fs, uint32* result) {
  error_code err = NO_ERROR;
  uint32 offset = 0;
  disk* d = fs->_.FAT121632.d;
  cache_block* cb;
  uint32 max_lba = fs->_.FAT121632.total_sectors;
  uint16 entries_per_sector = (1 << fs->_.FAT121632.log2_bps) >> 2;

  uint32 lba = fs->_.FAT121632.reserved_sectors;

  bool found = FALSE;
  // Inspect all sectors starting from *cluster
  // It is faster to use this method than repeated calls to
  // fat_32_get_fat_link_value since the latter will get a
  // cache block per request.
  uint32 clus;
  uint16 i;
  clus = i = 2;
  // We start from the first cluster because we have no clue where data has been
  // overridden
  while ((lba < max_lba) && !found) {
    if (ERROR(err = disk_cache_block_acquire(fs->_.FAT121632.d, lba, &cb))) {
      return err;
    }
    rwmutex_readlock(cb->mut);

    for (; i < entries_per_sector; ++i, ++clus) {
      uint32 entry = CAST(uint32*, cb->buf)[i];
      if (found = (entry == 0)) {
        break;
      }
    }

    rwmutex_readunlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
    ++lba;
    i = 0;
  }

  // Could not find an entry: disk out of space
  if (!found) err = DISK_OUT_OF_SPACE;
  *result = clus;
  return err;
}


static error_code fat_32_set_fat_link_value(fat_file_system* fs, uint32 cluster,
                                     uint32 value) {
  uint32 lba;
  cache_block* cb;
  disk* d = fs->_.FAT121632.d;
  ide_device* dev = d->_.ide.dev;
  uint16 entries_per_sector = (1 << fs->_.FAT121632.log2_bps) >> 2;
  error_code err;
  uint8 wrt_buff[4];

  if (cluster < 2) {
    panic(L"Cannot inspect lower than the second cluster entry");
  }

  { // Cache block section
    if (ERROR(err = disk_cache_block_acquire(d, 0, &cb))) return err;
    rwmutex_readlock(cb->mut);

    BIOS_Parameter_Block* p = CAST(BIOS_Parameter_Block*, cb->buf);
    lba = (cluster / entries_per_sector) + as_uint16(p->BPB_RsvdSecCnt);

    rwmutex_readunlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
  }

 
  uint32 offset_in_bytes = (cluster % entries_per_sector) << 2;

  // Read the cache in order to update it
  {  // Very important to lock this write.
    if (ERROR(err = disk_cache_block_acquire(d, lba, &cb))) return err;
    rwmutex_writelock(cb->mut);

    for (int i = 0; i < 4; ++i) {
      cb->buf[i + offset_in_bytes] = as_uint8(value, i);
    }

    cb->dirty = TRUE;
    rwmutex_writeunlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
  }

  return err;
}

error_code fat_32_create_empty_file(fat_file_system* fs, fat_file* parent_folder,
                                    native_char* name, fat_file** result) {
  FAT_directory_entry de;
  error_code err;
  native_char normalized_path[NAME_MAX + 1];
  native_string p = normalized_path;
  fat_file* f;

  // Section start is the LBA
  // We reset the file to make sure to find the first position
  fat_set_to_absolute_position(CAST(file*, parent_folder), 0);

  uint32 position = parent_folder->current_pos;
  while ((err = fat_read_file(CAST(file*, parent_folder), &de, sizeof(de))) == sizeof(de)) {
    // This means the entry is available
    if (de.DIR_Name[0] == 0) break;
    // Update the position with the information before
    position = parent_folder->current_pos;
  }

  if(ERROR(err)) {
    panic(L"Not been taken care of yet");
  }

  if(ERROR(err = new_fat_file(&f))) {
    return err;
  }

  if(NULL == f) {
    return MEM_ERROR;
  }

  // We recalculate the position to write the entry
  fat_set_to_absolute_position(CAST(file*, parent_folder), position);

  // We got a position for the root entry, we need to find an available FAT
  uint32 cluster = FAT32_FIRST_CLUSTER;

  if (ERROR(err = fat_32_find_first_empty_cluster(fs, &cluster))) {
    return err;
  }

  // Copy the short name
  memcpy(de.DIR_Name, name, FAT_NAME_LENGTH);

  {  // Set the cluster in the descriptor
    uint16 cluster_hi = (cluster & 0xFFFF0000) >> 16;
    uint16 cluster_lo = (cluster & 0x0000FFFF);

    for (int i = 0; i < 2; ++i) {
      de.DIR_FstClusHI[i] = as_uint8(cluster_hi, i);
      de.DIR_FstClusLO[i] = as_uint8(cluster_lo, i);
    }
  }

  if (ERROR(err = fat_write_file(CAST(file*, parent_folder), &de, sizeof(de)))) {
    return err;
  }

  if (ERROR(err = fat_32_set_fat_link_value(fs, cluster, FAT_32_EOF))) {
    return err;
  }

  // Correctly set to the right coordinates in the FAT
  // so we are at the beginning of the file
  f->fs = parent_folder->fs;
  f->first_cluster = f->current_cluster = cluster;
  f->current_section_length =
      1 << (fs->_.FAT121632.log2_bps + fs->_.FAT121632.log2_spc);
  f->current_section_start = ((cluster - 2) << fs->_.FAT121632.log2_spc) +
                             fs->_.FAT121632.first_data_sector;
  f->current_section_pos = 0;
  f->current_pos = 0;
  f->length = 0;
  // Set the file to the last position so we can easily write there
  // It is also the position of the directory entry so we set that at
  // the same time
  f->parent.first_cluster = parent_folder->first_cluster;
  f->entry.position = position;
  f->header.mode = S_IFREG;

  *result = f;

  return err;
}

static error_code fat_create_file(native_char* name, fat_file* parent_folder, fat_file** result) {
  error_code err;
  fat_file_system* fs = parent_folder->fs;

  switch (fs->kind) {
    case FAT12_FS:
    case FAT16_FS:
      panic(L"Not supported");
      break;
    case FAT32_FS: {
      err = fat_32_create_empty_file(fs, parent_folder, name, result);
    } break;
  }

  return err;
}

static error_code fat_update_file_length(fat_file* f) {
  // Update the directory entry
  // to set the correct length of the file
  error_code err = NO_ERROR;
  FAT_directory_entry de;
  uint32 filesize;

  fat_file* parent_dir;
  if(ERROR(err = new_fat_file(&parent_dir))) return err;

  parent_dir->fs = f->fs;
  parent_dir->first_cluster = f->parent.first_cluster;
  parent_dir->header.mode = S_IFDIR;

  fat_reset_cursor(parent_dir);
  fat_set_to_absolute_position(CAST(file*, parent_dir), f->entry.position);

  if (ERROR(err = fat_read_file(CAST(file*, parent_dir), &de, sizeof(de)))) {
    goto flush_file_update_dir_err_occured;
  }

  term_writeline(cout);
  for(int i =0; i< 11; ++i) {
    term_write(cout, CAST(native_char, de.DIR_Name[i]));
  }
  term_writeline(cout);

  // Go backwards to overwrite the directory entry
  fat_move_cursor(CAST(file*, parent_dir), -sizeof(de));

  filesize = f->length;
  
  for (int i = 0; i < 4; ++i) {
    de.DIR_FileSize[i] = as_uint8(filesize, i);
  }

  if (ERROR(err = fat_write_file(CAST(file*, parent_dir), &de, sizeof(de)))) {
    goto flush_file_update_dir_err_occured;
  }

flush_file_update_dir_err_occured:
  // Always close the file, even if there is an error
  error_code closing_err = fat_close_file(CAST(file*, parent_dir));
  return ERROR(err) ? err : closing_err;
}

static error_code fat_unlink_file(fat_file* f) {
  error_code err = NO_ERROR;

  uint32 cluster = f->first_cluster;
  uint32 next_clus;
  do {
    if(ERROR(err = fat_32_get_fat_link_value(f->fs, cluster, &next_clus))) break;
    if(ERROR(err = fat_32_set_fat_link_value(f->fs, cluster, NULL))) break;
  } while(next_clus != FAT_32_EOF && next_clus > 0);

  return err;
}

static error_code fat_truncate_file(fat_file* f) {
  error_code err = NO_ERROR;
  
  if(ERROR(err = fat_unlink_file(f))) return err;
  uint32 clus = f->first_cluster;
  
  if(ERROR(err = fat_32_set_fat_link_value(f->fs, clus, FAT_32_EOF))) return err;

  f->length = 0;
  
  if(ERROR(err = fat_update_file_length(f))) return err;

  return err;
}

error_code fat_open_file(native_string path, file_mode mode, file** result) {
  error_code err = NO_ERROR;
  fat_file_system* fs;
  native_char normalized_path[NAME_MAX + 1];
  native_string child_name;
  fat_file *parent,*child;

  if (fs_mod.nb_mounted_fs == 0) {
    // No file system mounted
    return UNKNOWN_ERROR;
  } else {
    fs = fs_mod.mounted_fs[0];
  }

  switch (fs->kind)
  {
  case FAT12_FS:
  case FAT16_FS:
  case FAT32_FS:
    break;
  
  default:
    return UNIMPL_ERROR;
    break;
  }

  if (ERROR(err = normalize_path(path, normalized_path))) {
#ifdef SHOW_DISK_INFO
    term_write(cout, "Failed to normalize the path\n\r");
#endif
    return err;
  }

  if (ERROR(err = fat_open_root_dir(fs, CAST(file**, &parent)))) {
#ifdef SHOW_DISK_INFO
    term_write(cout, "Error loading the root dir: ");
    term_write(cout, err);
    term_writeline(cout);
#endif
    return err;
  }

  // Find the parent folder
  uint8 depth;
  short_file_name_struct* parts = decompose_path(normalized_path, &depth);

  if(0 == depth) {
    return FNF_ERROR;
  }


  for(uint8 i = 0; i < depth - 1; ++i) {
    // Go get the actual parent folder
    if(ERROR(err = fat_fetch_entry(fs, parent, parts[i].name, &child))) {
      break;
    } else {
      fat_close_file(CAST(file*, parent));
      parent = child;
      child = NULL;
    }
  }

  if(ERROR(err)) {
    if(NULL != parent) fat_close_file(CAST(file*, parent));
    return err;
  }
  
  child_name = parts[depth - 1].name;
  if (HAS_NO_ERROR(err = fat_fetch_entry(fs, parent, child_name, &child))) {
    fat_set_to_absolute_position(CAST(file*, parent), 0);
    fat_set_to_absolute_position(CAST(file*, child), 0);
  }
  
  if (ERROR(err) && FNF_ERROR != err) {
    if (NULL != parent) fat_close_file(CAST(file*, parent));
    return err;
  }


  // If it is a directory, there is not mode
  if (!S_ISDIR(child->header.mode)) {
  // Set the file mode
    switch (mode) {
      case MODE_READ:
      case MODE_READ_WRITE: {
        if (ERROR(err)) return err;
        // otherwise everything is ok, there is nothing to
        // do in this mode beside having the cursor at the start.
      } break;

      case MODE_TRUNC:
      case MODE_TRUNC_PLUS: {
        if (ERROR(err)) {
          if (FNF_ERROR == err) {
            // Create the file
            if (ERROR(err = fat_create_file(child_name, parent, &child))) {
              return err;
            }
          } else {
            return err;
          }
        } else {
          if (ERROR(err = fat_truncate_file(child))) {
            return err;
          }
        }
      } break;

      case MODE_APPEND:
      case MODE_APPEND_PLUS: {
        if (ERROR(err)) {
          if (FNF_ERROR == err) {
            if (ERROR(err = fat_create_file(child_name, parent, &child))) {
              return err;
            }
          } else {
            return err;
          }
        } else {
          if (ERROR(err = fat_set_to_absolute_position(CAST(file*, child),
                                                        child->length - 1))) {
            return err;
          }
        }
      } break;
      default:
        panic(L"Unhandled file mode");
        break;
    }
  }
  
  if(NULL != parent) fat_close_file(CAST(file*, parent));

  *result = CAST(file*, child);

  return NO_ERROR;
}

static size_t fat_file_len(file* ff) {
  fat_file* f = CAST(fat_file*, ff);
  return CAST(size_t, f->length);
}

static dirent* fat_readdir(DIR* dir) {
  fat_file* f = CAST(fat_file*, dir->f);
  error_code err;

  switch (f->fs->kind) {
    case FAT12_FS:
    case FAT16_FS:
    case FAT32_FS: {
      FAT_directory_entry de;

      while ((err = fat_read_file(CAST(file*, f), &de, sizeof(de))) == sizeof(de)) {
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

error_code init_fat() {

  // Init the VTable
  _fat_vtable._file_close = fat_close_file;
  _fat_vtable._file_move_cursor = fat_move_cursor;
  _fat_vtable._file_read = fat_read_file;
  _fat_vtable._file_set_to_absolute_position = fat_set_to_absolute_position;
  _fat_vtable._file_write = fat_write_file;
  _fat_vtable._file_len = fat_file_len;
  _fat_vtable._readdir = fat_readdir;

  disk_add_all_partitions();
  mount_all_partitions();

  debug_write("FAT INIT");
  return NO_ERROR;
}

// file: "fs.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 Nov 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "fs.h"
#include "fat32.h"
#include "ide.h"
#include "disk.h"
#include "rtlib.h"
#include "term.h"

//-----------------------------------------------------------------------------

#define MAX_NB_MOUNTED_FS 8

typedef struct fs_module_struct
  {
    file_system* mounted_fs[MAX_NB_MOUNTED_FS];
    uint32 nb_mounted_fs;
  } fs_module;

static fs_module fs_mod;

//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------

// FAT file system implementation.

#define FAT_ATTR_READ_ONLY 0x01
#define FAT_ATTR_HIDDEN    0x02
#define FAT_ATTR_SYSTEM    0x04
#define FAT_ATTR_VOLUME_ID 0x08
#define FAT_ATTR_DIRECTORY 0x10
#define FAT_ATTR_ARCHIVE   0x20
#define FAT_ATTR_LONG_NAME \
(FAT_ATTR_READ_ONLY | FAT_ATTR_HIDDEN | FAT_ATTR_SYSTEM | FAT_ATTR_VOLUME_ID)

#define FAT_NAME_LENGTH 11

#define FAT_DIR_ENTRY_SIZE 32

typedef struct FAT_directory_entry_struct
  {
    uint8 DIR_Name[FAT_NAME_LENGTH];
    uint8 DIR_Attr;
    uint8 DIR_NTRes;
    uint8 DIR_CrtTimeTenth;
    uint8 DIR_CrtTime[2];
    uint8 DIR_CrtDate[2];
    uint8 DIR_LstAccDate[2];
    uint8 DIR_FstClusHI[2];
    uint8 DIR_WrtTime[2];
    uint8 DIR_WrtDate[2];
    uint8 DIR_FstClusLO[2];
    uint8 DIR_FileSize[4];
  } FAT_directory_entry;

  static error_code open_root_dir(file_system* fs, file** result) {
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
        if (ERROR(err = fat_32_open_root_dir(fs, f, result))) {
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

        while ((err = read_file(f, &de, sizeof(de))) == sizeof(de)) {
          if (de.DIR_Name[0] == 0) break;
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
              f->current_section_start =
                  fs->_.FAT121632.first_data_sector +
                  ((f->current_cluster - 2) << fs->_.FAT121632.log2_spc);
              f->current_section_length =
                  1 << (fs->_.FAT121632.log2_bps + fs->_.FAT121632.log2_spc);
              f->current_section_pos = 0;
              f->current_pos = 0;
              f->length = as_uint32(de.DIR_FileSize);
              if (de.DIR_Attr & FAT_ATTR_DIRECTORY)
                f->mode = S_IFDIR;
              else
                f->mode = S_IFREG;
              goto found;
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

error_code close_file (file* f)
{
  kfree (f);
  return NO_ERROR;
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
      debug_write("Reading FAT16 FAT section");
      offset = n * 2;
    } else {
      debug_write("Reading FAT32 FAT section");
      offset = n * 4;
    }

    sector_pos =
        fs->_.FAT121632.reserved_sectors + (offset >> fs->_.FAT121632.log2_bps);

    offset &= ~(~0U << fs->_.FAT121632.log2_bps);

    if (ERROR(err = disk_cache_block_acquire(fs->_.FAT121632.d, sector_pos,
                                             &cb))) {
      debug_write("Failed to aquire disk cache block");
      debug_write(sector_pos);
      debug_write(offset);
      return err;
    }

    if (fs->kind == FAT16_FS) {
      cluster = *CAST(uint16*, cb->buf + offset);
      if (ERROR(err = disk_cache_block_release(cb))) return err;
      if (cluster >= 0xfff8) return EOF_ERROR;
    } else {
      cluster = *CAST(uint16*, cb->buf + offset) & 0x0fffffff;
      if (ERROR(err = disk_cache_block_release(cb))) return err;
      if (cluster >= 0x0ffffff8) return EOF_ERROR;
    }
  }

  f->current_cluster = cluster;
  f->current_section_length =
      1 << (fs->_.FAT121632.log2_bps + fs->_.FAT121632.log2_spc);
  f->current_section_pos = 0;

  return NO_ERROR;
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

          if (f->current_section_pos >= f->current_section_length)
            if (ERROR(err = next_FAT_section(f))) {
              if (err != EOF_ERROR) return err;
              break;
            }

          left1 = f->current_section_length - f->current_section_pos;

          if (left1 > n) left1 = n;

          while (left1 > 0) {
            cache_block* cb;

            if (ERROR(err = disk_cache_block_acquire(
                          fs->_.FAT121632.d,
                          f->current_section_start +
                              (f->current_section_pos >> DISK_LOG2_BLOCK_SIZE),
                          &cb)))
              return err;

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

static error_code mount_FAT121632 (disk* d, file_system** result)
{
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

  if (ERROR(err = disk_cache_block_acquire (d, 0, &cb)))
    return err;

  p = CAST(BIOS_Parameter_Block*,cb->buf);


  bps = as_uint16(p->BPB_BytsPerSec);
  log2_bps = log2 (bps);

  spc = p->BPB_SecPerClus;
  log2_spc = log2 (spc);

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
    term_write(cout, bps); term_writeline(cout);
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
    case 1:  // Primary DOS 12-bit FAT
    case 4:  // Primary DOS 16-bit FAT
    case 6:  // Primary big DOS >32Mb
    case 0x0C: // FAT32 LBA

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

static void mount_all_partitions ()
{
  uint32 index;
  disk* d;

  fs_mod.nb_mounted_fs = 0;

  index = 0;

  while ((d = disk_find (index)) != NULL)
    {
      mount_partition (d);
      index++;
    }
}

//-----------------------------------------------------------------------------

DIR* opendir (native_string path)
{
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

static native_string copy_without_trailing_spaces
  (uint8* src,
   native_string dst,
   uint32 n)
{
  uint32 i;
  uint32 end = 0;

  for (i=0; i<n; i++)
    {
      dst[i] = src[i];
      if (src[i] != ' ')
        end = i+1;
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

error_code closedir (DIR* dir)
{
  close_file (dir->f); // ignore error
  kfree (dir); // ignore error

  return NO_ERROR;
}

//-----------------------------------------------------------------------------

error_code stat (native_string path, struct stat* buf)
{
  file* f;
  error_code err;

  if (ERROR(err = open_file (path, &f)))
    return err;

  buf->st_mode = f->mode;
  buf->st_size = f->length;

  return close_file (f);
}

//-----------------------------------------------------------------------------

void setup_fs ()
{
  disk_add_all_partitions ();
  mount_all_partitions ();
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //

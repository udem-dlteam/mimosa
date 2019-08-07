#include "general.h"
#include "rtlib.h"
#include "ide.h"
#include "disk.h"
#include "fat32.h"
#include "include/vfs.h"
#include "include/fat.h"

// -------------------------------------------------------------
// Declarations for the mounted FS systems
// -------------------------------------------------------------

#define MAX_NB_MOUNTED_FS 8

typedef struct fs_module_struct {
  fat_file_system* mounted_fs[MAX_NB_MOUNTED_FS];
  uint32 nb_mounted_fs;
} fs_module;

static fs_module fs_mod;

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



error_code init_fat() {
  disk_add_all_partitions();
  mount_all_partitions();
  return NO_ERROR;
}
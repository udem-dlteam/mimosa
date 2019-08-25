#include "include/fat.h"
#include "chrono.h"
#include "disk.h"
#include "general.h"
#include "ide.h"
#include "include/vfs.h"
#include "rtlib.h"
#include "thread.h"

// -------------------------------------------------------------
// Declarations for the mounted FS systems
// -------------------------------------------------------------

#define MAX_NB_MOUNTED_FAT_FS 8

static native_string DOT_NAME = ".          ";
static native_string DOT_DOT_NAME = "..         ";

typedef struct fs_module_struct {
  fat_file_system* mounted_fs[MAX_NB_MOUNTED_FAT_FS];
  uint32 nb_mounted_fs;
} fs_module;

static fat_open_chain start_sentinel;
static fat_open_chain end_sentinel;

static fs_module fs_mod;
static file_vtable _fat_file_vtable;
static fs_vtable _fat_vtable;

static error_code new_fat_file(fat_file** result) {
  error_code err = NO_ERROR;
  fat_file* allocated = CAST(fat_file*, kmalloc(sizeof(fat_file)));

  if(NULL == allocated) {
    err = MEM_ERROR;
    return err;
  }

  allocated->header._vtable = &_fat_file_vtable;
  allocated->header.name = NULL;
  allocated->header.mode = 0;
  allocated->header.type = 0;
  allocated->header._fs_header = NULL;

  allocated->link = NULL;
  allocated->first_cluster = 0;
  allocated->current_cluster = 0;
  allocated->current_section_start = 0;
  allocated->current_section_length = 0;
  allocated->current_pos = 0;
  allocated->length = 0;
  allocated->parent.first_cluster = 0;
  allocated->entry.position = 0;

  *result = allocated;

  return err;
}

static error_code fat_remove(fs_header* header, file* file);
static error_code fat_rename(fs_header* header, file* source, native_string name, uint8 depth);
static error_code fat_mkdir(fs_header* header,native_string name, uint8 depth, file** result);
static error_code fat_file_open(fs_header* header, native_string parts, uint8 depth, file_mode mode, file** result);
static error_code fat_stat(fs_header* header, file* f, stat_buff* buf);
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
static error_code fat_fetch_first_empty_directory_position(
    fat_file* directory, uint32* position, uint8 required_spots);
static error_code fat_fetch_file(fat_file* parent,
                                  native_char* name, fat_file** result);
static size_t fat_file_len(file* f);

static fat_open_chain* fat_chain_fetch(fat_file_system* fs, uint32 cluster);
static error_code fat_chain_add(fat_open_chain* link);
static error_code fat_chain_del(fat_open_chain* link);

static error_code fat_actual_remove(fat_file_system* fs, fat_file* f);
static fat_open_chain* new_chain_link(fat_file_system* fs, fat_file* file);
static error_code fat_allocate_directory_entry(
    fat_file_system * fs, fat_file * parent_folder, FAT_directory_entry * de,
    native_char * name, uint32* position);
static uint8 lfn_checksum(uint8* name_entry);
static void name_to_short_file_name(native_string n, short_file_name* result);
static error_code fat_fetch_parent(fat_file_system* fs, native_string* _parts, uint8 depth,
                                   fat_file** result);

static error_code read_lfn(fs_header* fs, uint32 cluster, uint32 entry_position,
                    native_string* result) ;

// -------------------------------------------------------------
// Mounting routines
// -------------------------------------------------------------

static error_code mount_FAT121632(disk* d, fat_file_system** result) {
  fat_file_system* fs = NULL;
  BIOS_Parameter_Block* p = NULL;
  bool expecting_FAT32 = FALSE;
  uint16 bps = 0;
  uint8 log2_bps = 0;
  uint16 spc = 0;
  uint8 log2_spc = 0;
  uint16 rec = 0;
  uint16 total_sectors16 = 0;
  uint32 total_sectors = 0;
  uint32 FAT_size = 0;
  uint32 reserved_sectors = 0;
  uint32 root_directory_sectors = 0;
  uint32 first_data_sector = 0;
  uint32 total_data_sectors = 0;
  uint32 total_data_clusters = 0;
  uint8 kind = 0;
  cache_block* cb = NULL;
  error_code err = NO_ERROR, release_err = NO_ERROR;

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

  if (NULL == fs) {
    err = MEM_ERROR;
  } else {
    fs->header.kind = FAT;
    fs->header._vtable = &_fat_vtable;
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

static uint16 pack_into_fat_time(uint8 hours, uint8 minutes, uint8 seconds) {
  uint16 packed_time = 0;
  // According to the FAT specification, the FAT time is set that way:
  // Bits 15-11: the hour
  // Bits 10-5 : the minutes
  // Bits 4-0  : the half seconds

  packed_time |= (hours & 0x1F) << 11;
  packed_time |= (minutes & 0x3F) << 5;
  packed_time |= ((seconds >> 1) & 0x1F);

  return packed_time;
}

static void unpack_fat_time(uint16 fat_time, uint8* hours, uint8* minutes, uint8* seconds) {
  // According to the FAT specification, the FAT time is set that way:
  // Bits 15-11: the hour
  // Bits 10-5 : the minutes
  // Bits 4-0  : the half seconds
  *hours = (fat_time >> 11) & 0x1F;
  *minutes = ((fat_time) >> 5) & 0x3F;
  *seconds = ((fat_time)) & 0x1F;
}

static uint16 pack_into_fat_date(int16 year, uint8 month, int8 day) {
  uint16 packed_date = 0;
  // According to the FAT specification, the FAT date is set that way:
  // Bits 15-9: Year relative to 1980
  // Bits 8-5: Month
  // Bits 4-0: Day of month

  if (year < 1980)
    year = 1980;  // This is a dirty hack but this is an error value

  packed_date |= ((year - 1980) & 0x7F) << 9;
  packed_date |= (month & 0xF) << 5;
  packed_date |= (day & 0x1F);

  return packed_date;
}

static void unpack_fat_date(uint16 fat_date, int16* year, uint8* month, uint8* day) {
  // According to the FAT specification, the FAT date is set that way:
  // Bits 15-9: Year relative to 1980
  // Bits 8-5: Month
  // Bits 4-0: Day of month
  *year = ((fat_date >> 9) & 0x7F) + 1980;
  *month = (fat_date >> 5) & 0xF;
  *day = (fat_date) & 0x1F;
}

static error_code mount_partition(disk* d, vfnode* parent) {
  native_string disk_name = "DSK1";
  fat_file_system* fs = NULL;
  error_code err = NO_ERROR;

  switch (d->partition_type) {
    case 1:     // Primary DOS 12-bit FAT
    case 4:     // Primary DOS 16-bit FAT
    case 6:     // Primary big DOS >32Mb
    case 0x0C:  // FAT32 LBA

      if (ERROR(err = mount_FAT121632(d, &fs))) {
        term_write(cout, "Failed to mount\n\r");
        return err;
      }

      vfnode* partition_mount_point = CAST(vfnode*, kmalloc(sizeof(vfnode)));

      if (NULL == new_vfnode(partition_mount_point, disk_name, TYPE_MOUNTPOINT)) {
        err = MEM_ERROR;
      } else {
        partition_mount_point->name[3] += fs_mod.nb_mounted_fs;
        partition_mount_point->_value.mountpoint.mounted_fs =
            CAST(fs_header*, fs);
        vfnode_add_child(parent, partition_mount_point);
      }
      break;
    default:
      term_write(cout, "Unknown partition type: ");
      term_write(cout, d->partition_type);
      term_write(cout, "\n\r");
      break;
  }

  if (!ERROR(err) && fs != NULL) {
    if (fs_mod.nb_mounted_fs < MAX_NB_MOUNTED_FAT_FS) {
      fs_mod.mounted_fs[fs_mod.nb_mounted_fs++] = fs;

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
        default:
          panic(L"Unknown FAT FS");
          break;
      }

      return NO_ERROR;
    }
  }

  if(HAS_NO_ERROR(err)) err = UNKNOWN_ERROR;
  
  return err;
}

static void mount_all_partitions(vfnode* parent) {
  uint32 index = 0;
  disk* d = NULL;
  fs_mod.nb_mounted_fs = 0;

  while ((d = disk_find(index)) != NULL) {
    mount_partition(d, parent);
    index++;
  }
}


// ------------------------------------------------------
// FAT manipulation routines
// ------------------------------------------------------

/*
Read a section of a LFN entry. The lfn section pointer points towards 
the LFN entry's section to read. The max len indicates the number of
bytes to read from the section (the section length). The buffer is the
string buffer where the characters are inserted and the index pointer
is used to keep track of the index where to write in the buffer. It is
updated after the section has been read. Since LFN sections are inserted
backwards, this procedure reads characters backwards into the buffer so
the buffer can be used as a string afterwards.
*/
static inline void read_lfn_section(uint8* lfn_section, uint8 maxlen,
                                    native_string buff, uint8* _index) {
  native_char c;
  uint8 index = *_index;
  for (int8 k = maxlen - 2; k >= 0; k -= 2) {
    if (0xFF != lfn_section[k] || 0xFF != lfn_section[k + 1]) {
      c = 0xFF & lfn_section[k];
      buff[index--] = (c >= 'a' && c <= 'z') ? (c - 32) : c;
    }
  }
  *_index = index;
}

#define FAT_IS_ROOT_DIR(f) ((f)->first_cluster <= FAT32_FIRST_CLUSTER)

static error_code fat_open_directory_entry(fat_file* f,
                                           FAT_directory_entry* de) {
  error_code err = NO_ERROR;
  fat_file* parent_dir = NULL;

  if (ERROR(err = new_fat_file(&parent_dir))) return err;

  parent_dir->header._fs_header = f->header._fs_header;
  parent_dir->first_cluster = f->parent.first_cluster;
  parent_dir->header.type = TYPE_FOLDER;

  fat_reset_cursor(CAST(file*, parent_dir)); // might seem useless but it actually initialize the cursor correctly
  fat_set_to_absolute_position(CAST(file*, parent_dir), f->entry.position);

  if (ERROR(err = fat_read_file(CAST(file*, parent_dir), de,
                                sizeof(FAT_directory_entry)))) {
    // nothing
  }

  fat_close_file(CAST(file*, parent_dir));

  return err;
}

static error_code fat_write_directory_entry(fat_file* f,
                                            FAT_directory_entry* de) {
  error_code err = NO_ERROR;
  fat_file* parent_dir = NULL;

  if (ERROR(err = new_fat_file(&parent_dir))) return err;

  parent_dir->header._fs_header = f->header._fs_header;
  parent_dir->first_cluster = f->parent.first_cluster;
  parent_dir->header.type = TYPE_FOLDER;

  // Correctly locate the DE to overwrite 
  fat_reset_cursor(CAST(file*, parent_dir)); // might seem useless but it actually initialize the cursor correctly
  fat_set_to_absolute_position(CAST(file*, parent_dir), f->entry.position);

  if (ERROR(err = fat_write_file(CAST(file*, parent_dir), de,
                                 sizeof(FAT_directory_entry)))) {
    return err;
  }

  fat_close_file(CAST(file*, parent_dir));

  return err;
}

static error_code fat_rename(fs_header* ffs, file* ff, native_string name,
                             uint8 depth) {
  error_code err = NO_ERROR;
  fat_file_system* fs = CAST(fat_file_system*, ffs);
  fat_file* f = CAST(fat_file*, ff);
  fat_file *target_parent = NULL, *parent_dir = NULL;
  FAT_directory_entry de;

  if (0 == depth) return FNF_ERROR;

  if (ERROR(err = fat_fetch_parent(fs, &name, depth, &target_parent))) {
    return err;
  }

  if (!IS_FOLDER(target_parent->header.type)) {
    return ARG_ERROR;  // the file paths are incorrect
  }

  if (ERROR(err = fat_open_directory_entry(f, &de))) {
    return err;
  }

  // Prepare the SFN
  short_file_name sfe;
  name_to_short_file_name(name, &sfe);
  // Copy the short name
  memcpy(de.DIR_Name, sfe.name, FAT_NAME_LENGTH);

  uint32 new_pos;
  if (ERROR(err = fat_allocate_directory_entry(fs, target_parent, &de, name,
                                               &new_pos))) {
    return err;
  }

  de.DIR_Name[0] = FAT_UNUSED_ENTRY;  // Set the entry available
  // Clean the old entry
  uint32 old_name_len = kstrlen(f->header.name);

  if (old_name_len > FAT_NAME_LENGTH) {
    // We need to overwrite the old entries
    uint8 no_of_entries = (old_name_len / FAT_CHARS_PER_LONG_NAME_ENTRY) +
                          (old_name_len % FAT_CHARS_PER_LONG_NAME_ENTRY != 0);

    if (ERROR(err = new_fat_file(&parent_dir))) return err;

    parent_dir->header._fs_header = f->header._fs_header;
    parent_dir->first_cluster = f->parent.first_cluster;
    parent_dir->header.type = TYPE_FOLDER;

    // Correctly locate the DE to overwrite
    fat_reset_cursor(
        CAST(file*, parent_dir));  // might seem useless but it actually
                                   // initialize the cursor correctly
    fat_set_to_absolute_position(
        CAST(file*, parent_dir),
        f->entry.position - (sizeof(long_file_name_entry) * no_of_entries));

    while (no_of_entries-- > 0) {
      if (ERROR(err =
                    fat_write_file(CAST(file*, parent_dir), &de, sizeof(de)))) {
        goto fat_rename_end;
      }
    }
  }
  // The eager eye might have noticed that we don't copy over the same
  // entry back where we originally read it. It doesn't matter, since
  // the old entry is now garbage anyways.
  if (ERROR(err = fat_write_directory_entry(f, &de))) {
    goto fat_rename_end;
  }

fat_rename_end:
  // Update the information to be able to quickly find the root dir
  f->parent.first_cluster = target_parent->first_cluster;
  f->entry.position = new_pos;
  fat_close_file(CAST(file*, target_parent));

  if(NULL != parent_dir) {
    fat_close_file(CAST(file*, parent_dir));
  }

  return err;
}

error_code fat_close_file(file* ff) {
  fat_file_system* fs = CAST(fat_file_system*, ff->_fs_header);
  fat_file* f = CAST(fat_file*, ff);

  if (NULL != ff->name) kfree(ff->name);

  if (NULL != f->link) {
    fat_open_chain* link = f->link;
    link->ref_count--;

    if (0 == link->ref_count && link->remove_on_close) {
      fat_actual_remove(fs, f);
      fat_chain_del(link);
      kfree(link);
    }
  }

  kfree(f);
  return NO_ERROR;
}

/*
 Reset the cursor of a fat file. This will also
 correctly initialize it and is safe to call on
 fat files that are not completly initialized. The 
 only fields that are required are the first cluster
 and the file system pointer.
*/
static void fat_reset_cursor(file* ff) {
  fat_file* f = CAST(fat_file*, ff);
  fat_file_system* fs = CAST(fat_file_system*, f->header._fs_header);

  f->current_cluster = f->first_cluster;
  f->current_section_length =
      1 << (fs->_.FAT121632.log2_bps + fs->_.FAT121632.log2_spc);
  f->current_section_start =
      ((f->first_cluster - 2) << fs->_.FAT121632.log2_spc) +
      fs->_.FAT121632.first_data_sector;
  f->current_section_pos = 0;
  f->current_pos = 0;
}

/*
 Move a file cursor to the next FAT section. It will cross
 the boundaries of a cluster.
*/
static error_code next_FAT_section(fat_file* f) {
  fat_file_system* fs = CAST(fat_file_system*, f->header._fs_header);
  uint32 n = f->current_cluster;
  uint32 offset;
  uint32 sector_pos;
  uint32 cluster;
  cache_block* cb = NULL;
  error_code err;

  if (fs->kind == FAT12_FS) {
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
      // term_writeline(cout);
      // term_write(cout, "A");
      if (ERROR(err = disk_cache_block_acquire(fs->_.FAT121632.d, sector_pos,
                                               &cb))) {
        return err;
      }
      // term_write(cout, "]");

      {
        // term_write(cout, "L");
        rwmutex_readlock(cb->mut);
        // term_write(cout, "]");

        if (fs->kind == FAT16_FS) {
          cluster = *CAST(uint16*, cb->buf + offset);
          if (cluster >= 0xfff8) err = EOF_ERROR;
        } else {
          cluster = *CAST(uint32*, cb->buf + offset) & 0x0fffffff;
          if (cluster >= FAT_32_EOF) err = EOF_ERROR;
        }

        // term_write(cout, "R");
        rwmutex_readunlock(cb->mut);
        // term_write(cout, "]");
      }

      // term_write(cout, "D");
      error_code release_err = disk_cache_block_release(cb);
      // term_write(cout, "]");

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

/*
Set the cursor position to the position in parameter, but starting the
mouvements from the absolute start of the file (it will set the cursor to zero,
then move to the position). This is useful to perform cluster boundary-crossing
mouvements backwards, but is slower than doing small relatives movements (not
always possible).
*/
static error_code fat_file_set_pos_from_start(fat_file* f, uint32 position) {
  error_code err, release_error = NO_ERROR;
  fat_file_system* fs = CAST(fat_file_system*,f->header._fs_header);
  BIOS_Parameter_Block* p = NULL;
  cache_block* cb = NULL;
  disk* d = fs->_.FAT121632.d;

  if (IS_REGULAR_FILE(f->header.type) && (position > f->length)) {
    return ARG_ERROR;
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

    fat_reset_cursor(CAST(file*, f));
    // We are now at the beginning of the file.
    // We want to go to the position wanted, so
    // we walk through the FAT chain until we read
    // as many clusters as required to get a correct position.
    for (uint32 i = 0; i < no_of_clusters; ++i) {
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

/*
Move the cursor of a fat file by a movement 'n'. A negative movement
indicates a "backwards" movement in the file buffer where as a positive
movement indicates a "forward" movement in the same file.
*/
static error_code fat_move_cursor(file* ff, int32 n) {
  fat_file* f = CAST(fat_file*, ff);
  uint32 displacement;
  BIOS_Parameter_Block* p = NULL;
  cache_block* cb = NULL;
  error_code err = NO_ERROR;
  fat_file_system* fs = CAST(fat_file_system*, f->header._fs_header);
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

        for (uint32 i = 0; i < no_of_clusters; ++i) {
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

/*
Set the cursor of a fat file to an absolute position, relative to 0, the start
of the file
*/
static error_code fat_set_to_absolute_position(file* ff, uint32 position) {
  fat_file* f = CAST(fat_file*, ff);

  if(f->current_pos == position) {
    return NO_ERROR;
  }

  if (0 == position) {
    fat_reset_cursor(ff);
    return NO_ERROR;
  }

  int32 mvmt = position - f->current_pos;
  return fat_move_cursor(CAST(file*, f), mvmt);
}

error_code fat_write_file(file* ff, void* buff, uint32 count) {
  fat_file* f = CAST(fat_file*, ff);
  error_code err = NO_ERROR;
  fat_file_system* fs = CAST(fat_file_system*, f->header._fs_header);
  
  if (NULL == buff) return ARG_ERROR;
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

  if (!ERROR(err) && !IS_FOLDER(f->header.type)) {
    f->length += count;
    err = fat_update_file_length(f);
  }

  return err;
}

error_code fat_read_file(file* ff, void* buf, uint32 count) {
  fat_file* f = CAST(fat_file*, ff);
  if (count > 0) {
    fat_file_system* fs = CAST(fat_file_system*, f->header._fs_header);
    error_code err;

    switch (fs->kind) {
      case FAT12_FS:
      case FAT16_FS:
      case FAT32_FS: {
#ifdef SHOW_FILE_READ_PROGRESS
        uint8 progress_counter = 0;
#endif
        uint32 n;
        uint8* p;

        if (!IS_FOLDER(f->header.type)) {
          uint32 left = f->length - f->current_pos;
          if (count > left) count = left;
        }

        n = count;
        p = CAST(uint8*, buf);

        while (n > 0) {
#ifdef SHOW_FILE_READ_PROGRESS
          if ((progress_counter % 200) == 0) {
            term_write(cout, "Reading progress: ");
            term_write(cout, count - n);
            term_write(cout, " / ");
            term_write(cout, count);
            term_writeline(cout);
          }
          progress_counter = (progress_counter + 1) % 200;
#endif
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
            left2 = (1 << DISK_LOG2_BLOCK_SIZE) -
                    (f->current_section_pos & ~(~0U << DISK_LOG2_BLOCK_SIZE));

            if (left2 > left1) left2 = left1;

            if (NULL != buf) {
              // term_writeline(cout);
              // term_write(cout, 'a');
              if (ERROR(
                      err = disk_cache_block_acquire(
                          fs->_.FAT121632.d,
                          f->current_section_start +
                              (f->current_section_pos >> DISK_LOG2_BLOCK_SIZE),
                          &cb))) {
                return err;
              }
              // term_write(cout, ')');

              {
                rwmutex_readlock(cb->mut);

                memcpy(p,
                       cb->buf + (f->current_section_pos &
                                  ~(~0U << DISK_LOG2_BLOCK_SIZE)),
                       left2);

                rwmutex_readunlock(cb->mut);
              }

              // term_write(cout, 'r');
              if (ERROR(err = disk_cache_block_release(cb))) return err;
              // term_write(cout, ')');
            }

            left1 -= left2;
            f->current_section_pos += left2;
            f->current_pos += left2;
            n -= left2;
            p += left2;  // We read chars, skip to the next part
          }
        }

#ifdef SHOW_FILE_READ_PROGRESS
        term_write(cout, "Reading done\n");
#endif
        return count - n;
      }

      default:
        return UNIMPL_ERROR;
    }
  }

  return NO_ERROR;
}

/*
Fetch a file of name "name" from the parent directory (parent).
The result is passed back and an error code is returned. 
*/
static error_code fat_fetch_file(fat_file* parent, native_string name,
                                 fat_file** result) {
  native_char lfn_buff[256];
  uint8 name_len;
  uint8 lfn_index = 254;
  short_file_name sfn_buff;
  uint8 name_match = 0;
  uint32 i = 0;
  error_code err = NO_ERROR;
  FAT_directory_entry de;
  fat_file* f = NULL;
  fat_file_system* fs = CAST(fat_file_system*, parent->header._fs_header);

  if ('\0' == name[0]) {
    return ARG_ERROR;
  }
  
  if (!IS_FOLDER(parent->header.type)) {
    return ARG_ERROR;
  }

  lfn_buff[255] = '\0';
  name_len = kstrlen(name);

  if (name_len <= FAT_NAME_LENGTH) {
    name_to_short_file_name(name, &sfn_buff);
  }

  uint32 cluster = parent->first_cluster;
  uint32 position = parent->current_pos;
  
  int16 checksum = -1;

#define invalidate_lfn() \
  do {                   \
    checksum = -1;       \
    lfn_index = 254;     \
  } while (0)

  while ((err = fat_read_file(CAST(file*, parent), &de, sizeof(de))) ==
         sizeof(de)) {
    if (de.DIR_Attr == FAT_ATTR_LONG_NAME && name_len > FAT_NAME_LENGTH) {
      // We want to write in the buffer since we are already over the entry
      long_file_name_entry* lfe = CAST(long_file_name_entry*, &de);

      if (checksum == -1 && !(lfe->LDIR_ord & FAT_LAST_LONG_ENTRY)) {
        invalidate_lfn();
      } else if (lfe->LDIR_ord & FAT_LAST_LONG_ENTRY && checksum != -1) {
        invalidate_lfn();
      } else if (checksum != lfe->LDIR_Checksum &&
                 !(lfe->LDIR_ord & FAT_LAST_LONG_ENTRY)) {
        invalidate_lfn();
      } else {
        checksum = lfe->LDIR_Checksum;
        // We read in reverse, because we start from the n'th entry to the
        // first one, as described in the fat specification
        read_lfn_section(lfe->LDIR_Name3, 4, lfn_buff, &lfn_index);
        read_lfn_section(lfe->LDIR_Name2, 12, lfn_buff, &lfn_index);
        read_lfn_section(lfe->LDIR_Name1, 10, lfn_buff, &lfn_index);
      }
    }

    if (de.DIR_Name[0] == 0) break;  // No more entries
    // Only verify the file if it's readable
    if (de.DIR_Name[0] != FAT_UNUSED_ENTRY && (de.DIR_Attr & FAT_ATTR_HIDDEN) == 0 &&
        (de.DIR_Attr & FAT_ATTR_VOLUME_ID) == 0) {

      if(name_len <= FAT_NAME_LENGTH) {
        for (i = 0; i < FAT_NAME_LENGTH; i++) {
          if (de.DIR_Name[i] != sfn_buff.name[i]) break;
        }
        name_match = (i == FAT_NAME_LENGTH);
      } else {
        native_string read_lfn = lfn_buff + lfn_index + 1;
        name_match = (0 == kstrcmp(read_lfn, name));
      }

      if (name_match) {
        // All the characters have been compared successfuly
        if(ERROR(err = new_fat_file(&f))) return err;

        f->header._fs_header = CAST(fs_header*, fs);
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
          f->header.type = TYPE_FOLDER;
        } else {
          f->header.type = TYPE_REGULAR;
        }

        // Setup the entry file. It is relative to the file's
        // directory
        f->parent.first_cluster = cluster;
        f->entry.position = position;

        f->header.name =
            CAST(native_string, kmalloc(sizeof(unicode_char) * (name_len + 1)));
        memcpy(f->header.name, name, name_len + 1);

        goto found;
      } else {
        invalidate_lfn();
      }
    }
    // Copy over the position information
    position = parent->current_pos;
  }

#undef invalidate_lfn
  // We did not find the file

  if (ERROR(err)) return err;
  return FNF_ERROR;

found:
  *result = f;
  return NO_ERROR;
}

static error_code fat_32_open_root_dir(fat_file_system* fs, fat_file* f) {
#ifdef SHOW_DISK_INFO
  term_write(cout, "Loading FAT32 root dir\n\r");
#endif
  cache_block* cb = NULL;
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

  f->header._fs_header = CAST(fs_header*, fs);
  f->first_cluster = f->current_cluster = root_cluster;

#ifdef SHOW_DISK_INFO
  term_write(cout, "Root cluster is: ");
  term_write(cout, root_cluster);
  term_writeline(cout);
#endif

  f->current_section_start = fs->_.FAT121632.first_data_sector;
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
  f->header.type = TYPE_FOLDER;

  return NO_ERROR;
}

static error_code fat_open_root_dir(fat_file_system* fs, file** result) {
  error_code err;
  fat_file* f = NULL;
  
  if(ERROR(err = new_fat_file(&f))) return err;

  if (NULL == f) return MEM_ERROR;

  switch (fs->kind) {
    case FAT12_FS:
    case FAT16_FS: {
#ifdef SHOW_DISK_INFO
      term_write(cout, "Opening FAT12/FAT16\n\r");
#endif
      f->header._fs_header = CAST(fs_header*, fs);
      f->current_cluster = 1;  // so that EOC is detected at end of dir
      f->current_section_start = fs->_.FAT121632.first_data_sector -
                                 fs->_.FAT121632.root_directory_sectors;
      f->current_section_length = fs->_.FAT121632.root_directory_sectors
                                  << fs->_.FAT121632.log2_bps;
      f->current_section_pos = 0;
      f->current_pos = 0;
      f->length = f->current_section_length;
      f->header.type = TYPE_FOLDER;

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

/*
Get a value of a link in the fat chain. The cluster number identifies the link
to get the value from. An error code is returned.
*/
error_code fat_32_get_fat_link_value(fat_file_system* fs, uint32 cluster,
                                     uint32* value) {
  uint32 lba;
  disk* d = fs->_.FAT121632.d;
  error_code err;
  cache_block* cb = NULL;
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

/*
Scan the FAT chain to find the first empty, usable cluster.
*/
static error_code fat_32_find_first_empty_cluster(fat_file_system* fs, uint32* result) {
  error_code err = NO_ERROR;
  cache_block* cb = NULL;
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
  // overwritten 
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

/*
Set a link value in the cluster chain. The cluster identifies the link. An
error code is returned in case of an error.
*/
static error_code fat_32_set_fat_link_value(fat_file_system* fs, uint32 cluster,
                                     uint32 value) {
  uint32 lba;
  cache_block* cb = NULL;
  disk* d = fs->_.FAT121632.d;
  uint16 entries_per_sector = (1 << fs->_.FAT121632.log2_bps) >> 2;
  error_code err;

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

/*
Find the section of a directory that can be used to allocate
"required_spots" directory entries. The file may be extended
to make place for those entries.
*/
static error_code fat_fetch_first_empty_directory_position(
    fat_file* directory, uint32* _position, uint8 required_spots) {
  FAT_directory_entry de;
  uint32 position = *_position;
  uint8 spots_left = required_spots;
  error_code err = NO_ERROR;

  if(directory->header.mode == MODE_READ) {
    return ARG_ERROR;
  }

  fat_reset_cursor(CAST(file*, directory));

  position = directory->current_pos;
  while (((err = fat_read_file(CAST(file*, directory), &de, sizeof(de))) == sizeof(de))) {
    // This means the entry is available
    if (de.DIR_Name[0] == FAT_UNUSED_ENTRY) {
      // Check if we have enough space
      if (0 == --spots_left) break;
      continue;
    }
    // This means all the following entries are available
    if (de.DIR_Name[0] == 0) break;
    // Update the position with the information before
    position = directory->current_pos;
    spots_left = required_spots;
  }

  if (EOF_ERROR == err) {
    de.DIR_Attr = 0;
    err = NO_ERROR;
    for (uint8 i = 0; i < required_spots; ++i) {
      if (ERROR(err = file_write(directory, &de, sizeof(de)))) {
        break;
      }
    }
  }

  *_position = position;

  return err;
}

/*
Write the directory entry de in the parent_folder. The name of the file
is passed in case that the file requires "long file name support". The
position pointer is updated with the position of the directory entry of the
file. The directory entry is expected to be filled completly.
*/
static error_code fat_allocate_directory_entry(fat_file_system* fs,
                                               fat_file* parent_folder,
                                               FAT_directory_entry* de,
                                               native_char* name,
                                               uint32* _position) {
  long_file_name_entry lfe;
  error_code err = NO_ERROR;

  uint8 checksum = lfn_checksum(de->DIR_Name);
  uint8 name_len = kstrlen(name);
  // We need to find enough empty directory entries to
  // allow the long file name to be stored.
  uint8 required_spots;

  if(name_len <= FAT_NAME_LENGTH) {
    required_spots = 0;
  } else {
    required_spots = (name_len / FAT_CHARS_PER_LONG_NAME_ENTRY) +
                     (name_len % FAT_CHARS_PER_LONG_NAME_ENTRY != 0);
  }

  ++required_spots;

  if(ERROR(err = fat_set_to_absolute_position(CAST(file*, parent_folder), 0))) {
    return err;
  } 

  uint32 position;
  if (ERROR(err = fat_fetch_first_empty_directory_position(
                parent_folder, &position, required_spots))) {
    return err;
  }

  if (ERROR(err = fat_set_to_absolute_position(CAST(file*, parent_folder),
                                               position))) {
    return err;
  }

  uint8 name_index = name_len;

  for (uint8 entry_idx = 0; entry_idx < (required_spots - 1); ++entry_idx) {
    uint8 ordinal = (required_spots - 1) - entry_idx;
    uint8 max_chars, chars;

    if (0 == entry_idx) {
      ordinal |= FAT_LAST_LONG_ENTRY;
      // Required spots - 2 because one is the directory entry and we don't count
      // the current long name entry (we want the number of chars in this specific
      // entry)
      max_chars = chars =
          name_len - ((required_spots - 2) * FAT_CHARS_PER_LONG_NAME_ENTRY);
    } else {
      max_chars = chars = FAT_CHARS_PER_LONG_NAME_ENTRY;
    }

    native_char c;
    // Write the LFN sections. This should be refactored...

    for (uint8 i = 0; i < 10; i += 2) {
      if (chars > 0) {
        c = name[name_index - (chars--)];
        lfe.LDIR_Name1[i] = c;
        lfe.LDIR_Name1[i + 1] = 0x00;
      } else {
        lfe.LDIR_Name1[i] = lfe.LDIR_Name1[i + 1] = 0xFF;
      }
    }

    for (uint8 i = 0; i < 12; i += 2) {
      if (chars > 0) {
        c = name[name_index - (chars--)];
        lfe.LDIR_Name2[i] = c;
        lfe.LDIR_Name2[i + 1] = 0x00;
      } else {
        lfe.LDIR_Name2[i] = lfe.LDIR_Name2[i + 1] = 0xFF;
      };
    }

    for (uint8 i = 0; i < 4; i += 2) {
      if (chars > 0) {
        c = name[name_index - (chars--)];
        lfe.LDIR_Name3[i] = c;
        lfe.LDIR_Name3[i + 1] = 0x00;
      } else {
        lfe.LDIR_Name3[i] = lfe.LDIR_Name3[i + 1] = 0xFF;
      }
    }

    name_index -= max_chars;

    lfe.LDIR_ord = ordinal;
    lfe.LDIR_Checksum = checksum;
    lfe.LDIR_Attr = FAT_ATTR_LONG_NAME;
    lfe.LDIR_FstClusLO[0] = lfe.LDIR_FstClusLO[1] = lfe.LDIR_Type = 0;

    if (ERROR(err = fat_write_file(CAST(file*, parent_folder), &lfe,
                                   sizeof(lfe)))) {
      return err;  // TODO check that this is not causing issues
    }
  }

  *_position = parent_folder->current_pos;

  if (ERROR(err = fat_write_file(CAST(file*, parent_folder), de,
                                 sizeof(FAT_directory_entry)))) {
    return err;
  }

  return err;
}

/*
Create an empty fat file in the folder "parent_folder". The directory
entrty "de" is filled by this function. The resulting file is placed into
"result".
*/
static error_code fat_32_create_empty_file(fat_file_system* fs,
                                           fat_file* parent_folder,
                                           FAT_directory_entry* de,
                                           native_char* name,
                                           uint8 attributes,
                                           fat_file** result) {
  error_code err;
  fat_file* f = NULL;

  if(ERROR(err = new_fat_file(&f))) {
    return err;
  }

  if(NULL == f) {
    return MEM_ERROR;
  }

  // Find a cluster to write the file into
  uint32 cluster = FAT32_FIRST_CLUSTER;

  if (ERROR(err = fat_32_find_first_empty_cluster(fs, &cluster))) {
    return err;
  }

  short_file_name sfe;
  name_to_short_file_name(name, &sfe);
  // Copy the short name
  memcpy(de->DIR_Name, sfe.name, FAT_NAME_LENGTH);

  {  // Set the cluster in the descriptor
    uint16 cluster_hi = (cluster & 0xFFFF0000) >> 16;
    uint16 cluster_lo = (cluster & 0x0000FFFF);

    for (int i = 0; i < 2; ++i) {
      de->DIR_FstClusHI[i] = as_uint8(cluster_hi, i);
      de->DIR_FstClusLO[i] = as_uint8(cluster_lo, i);
    }
  }

  for (uint8 i = 0; i < 4; ++i) {
    de->DIR_FileSize[i] = 0;
  }

  uint8 hours, minutes, seconds;
  int16 year;
  uint8 month, day;

  uint16 time, date;

  get_current_time(&hours, &minutes, &seconds);
  get_current_date(&year, &month, &day);

  time = pack_into_fat_time(hours, minutes, seconds);
  date = pack_into_fat_date(year, month, day);

  for(uint8 i = 0; i < 2; ++i) {
    de->DIR_CrtTime[i] = de->DIR_WrtTime[i] = as_uint8(time, i);
    de->DIR_CrtDate[i] = de->DIR_WrtDate[i] = as_uint8(date,i);
  }

  de->DIR_Attr = attributes;

  // -------------------------------------------------------------------------------
  // Write the directory entry to the disk, modifications must be done by this point
  // -------------------------------------------------------------------------------

  // Claim the link right now to avoid it being taken by the directory entry
  // if it needs to be enlarged
  if (ERROR(err = fat_32_set_fat_link_value(fs, cluster, FAT_32_EOF))) {
    return err;
  }

  uint32 position;
  if(ERROR(err = fat_allocate_directory_entry(fs, parent_folder, de, name, &position))) {
    return err; 
  }
  // Correctly set to the right coordinates in the FAT
  // so we are at the beginning of the file
  f->header._fs_header = parent_folder->header._fs_header;
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
  
  if(attributes & FAT_ATTR_DIRECTORY) {
    f->header.type = TYPE_FOLDER;
  } else {
    f->header.type = TYPE_REGULAR;
  }

  uint8 name_len = kstrlen(name);
  f->header.name = CAST(native_string, kmalloc(sizeof(native_char) * (name_len + 1)));
  memcpy(f->header.name, name, name_len + 1);

  *result = f;

  return err;
}

static error_code fat_create_file(native_char* name, fat_file* parent_folder, fat_file** result) {
  error_code err = NO_ERROR;
  FAT_directory_entry de;
  fat_file_system* fs = CAST(fat_file_system*, parent_folder->header._fs_header);

  switch (fs->kind) {
    case FAT12_FS:
    case FAT16_FS:
      panic(L"Not supported");
      break;
    case FAT32_FS: {
      // TODO: add correct attributes
      err = fat_32_create_empty_file(fs, parent_folder, &de, name, 0, result);
    } break;
  }

  return err;
}

/*
From the "_part" string (that represents a decomposed file path), fetch the parent of the last
item in the path (wheter it is a folder or a file). The depth represents the number of
levels in the path. The result is placed into result and an error code is returned. 
Exemple:
  File path: //parentA//parentB//parentC//parentD//file\0
  _parts:    \0parentA\0parentB\0parentC\0parentD\0file\0
  Depth: 5
  The last file is file, so the parent returned is parentD.
  For a depth of 4, the last file would be parentD, so the parent
  returned would be parentC.
This function is expected to move the _parts "cursor" forward up
until the beginning of the last part of the name. In the first example,
the _parts pointer would be:
                            file\0
at the end of the function call.
*/
static error_code fat_fetch_parent(fat_file_system* fs, native_string* _parts,
                                   uint8 depth, fat_file** result) {
  error_code err = NO_ERROR;
  native_string parts = *_parts;
  fat_file *parent = NULL, *child = NULL;

  if (ERROR(err = fat_open_root_dir(fs, CAST(file**, &parent)))) {
    return err;
  }

  if (0 == depth) {
    // Open the root directory
    *result = parent;
    return NO_ERROR;
  }

  for (uint8 i = 0; i < depth - 1; ++i) {
    // Go get the actual parent folder
    if (ERROR(err = fat_fetch_file(parent, parts, &child))) {
      break;
    } else {
      fat_close_file(CAST(file*, parent));
      parent = child;
      child = NULL;

      while (*parts++ != '\0')
        ; // Go to the next string in the "parts" string array
    }
  }

  if (ERROR(err) && NULL != parent) {
    fat_close_file(CAST(file*, parent));
  } else {
    *_parts = parts;
    *result = parent;
  }

  return err;
}

static error_code fat_mkdir(fs_header* ffs, native_string name, uint8 depth, file** result) {
  error_code err = NO_ERROR;
  fat_file_system* fs = CAST(fat_file_system*, ffs);
  fat_file *parent = NULL, *folder = NULL;

  switch (fs->kind) {
    case FAT12_FS:
    case FAT16_FS:
    case FAT32_FS:
      break;

    default:
      return UNIMPL_ERROR;
      break;
  }

  if (0 == depth) {
    return FNF_ERROR; // Need at least a folder name
  }

  if(ERROR(err = fat_fetch_parent(fs, &name, depth, &parent))) {
    return err;
  }

  if (HAS_NO_ERROR(err = fat_fetch_file(parent, name, &folder))) {
    // We expected a FNF
    if (NULL != parent) fat_close_file(CAST(file*, parent));
    return ARG_ERROR; // incorrect file since it exists already
  }

  FAT_directory_entry file_de;
  err = fat_32_create_empty_file(fs, parent, &file_de, name, FAT_ATTR_DIRECTORY,
                                 &folder);

  // Create the '.' entry and the '..' entry:
  if(HAS_NO_ERROR(err)) {
    FAT_directory_entry dot_dot_entry;

    if(FAT_IS_ROOT_DIR(parent)) {
      // Fill out dotdot manually
      dot_dot_entry.DIR_Attr = FAT_ATTR_DIRECTORY;

      dot_dot_entry.DIR_FstClusLO[0] = dot_dot_entry.DIR_FstClusLO[1] =
          dot_dot_entry.DIR_FstClusHI[0] = dot_dot_entry.DIR_FstClusHI[1] = 0;
      
      for(uint8 i = 0; i < 2; ++i) {
        // Copy the containing folder time properties
        dot_dot_entry.DIR_CrtDate[i] = file_de.DIR_CrtDate[i];
        dot_dot_entry.DIR_CrtTime[i] = file_de.DIR_CrtTime[i];
        dot_dot_entry.DIR_LstAccDate[i] = file_de.DIR_LstAccDate[i];
        dot_dot_entry.DIR_WrtDate[i] = file_de.DIR_WrtDate[i];
        dot_dot_entry.DIR_WrtTime[i] = file_de.DIR_WrtTime[i];
      }

      dot_dot_entry.DIR_CrtTimeTenth = file_de.DIR_CrtTimeTenth;
    } else if(ERROR(err = fat_open_directory_entry(parent, &dot_dot_entry))) {
      //error
    }

    // Overwrite the names of the entries
    for(uint8 i = 0; i < 11; ++i) {
      file_de.DIR_Name[i] = DOT_NAME[i];
      dot_dot_entry.DIR_Name[i] = DOT_DOT_NAME[i];
    }

    if(ERROR(err = file_write(folder, &file_de, sizeof(FAT_directory_entry)))) {
      // error
    } else if(ERROR(err = file_write(folder, &dot_dot_entry, sizeof(FAT_directory_entry)))) {
      // error
    }
  }

  if(NULL != parent) fat_close_file(CAST(file*, parent));

  *result = CAST(file*, folder);

  return err;
}

/* Update the file length of a file on disk */
static error_code fat_update_file_length(fat_file* f) {
  // Update the directory entry
  // to set the correct length of the file
  error_code err = NO_ERROR;
  FAT_directory_entry de;

  if(ERROR(err = fat_open_directory_entry(f, &de))) {
    return err;
  }

  uint8 hours, minutes, seconds;
  int16 year;
  uint8 month, day;

  uint16 time, date;

  get_current_time(&hours, &minutes, &seconds);
  get_current_date(&year, &month, &day);

  time = pack_into_fat_time(hours, minutes, seconds);
  date = pack_into_fat_date(year, month, day);

  for(uint8 i = 0; i < 2; ++i) {
    de.DIR_WrtTime[i] = as_uint8(time, i);
    de.DIR_WrtDate[i] = as_uint8(date,i);
  }

  for (uint8 i = 0; i < 4; ++i) {
    de.DIR_FileSize[i] = as_uint8(f->length, i);
  }

  err = fat_write_directory_entry(f, &de);

  return err;
}

/*
Unlink the chain for the file f. This is used
when deleting a file for instance.
*/
static error_code fat_unlink_file(fat_file* f) {
  error_code err = NO_ERROR;

  uint32 cluster = f->first_cluster;
  uint32 next_clus;
  do {
    if(ERROR(err = fat_32_get_fat_link_value(CAST(fat_file_system*, f->header._fs_header), cluster, &next_clus))) break;
    if(ERROR(err = fat_32_set_fat_link_value(CAST(fat_file_system*, f->header._fs_header), cluster, NULL))) break;
  } while(next_clus != FAT_32_EOF && next_clus > 0);

  return err;
}

/* Truncate a file to be of size 0 */
static error_code fat_truncate_file(fat_file* f) {
  error_code err = NO_ERROR;
  
  if(ERROR(err = fat_unlink_file(f))) return err;
  uint32 clus = f->first_cluster;
  
  if(ERROR(err = fat_32_set_fat_link_value(CAST(fat_file_system*, f->header._fs_header), clus, FAT_32_EOF))) return err;

  f->length = 0;
  if(ERROR(err = fat_update_file_length(f))) return err;

  return err;
}

error_code fat_file_open(fs_header* ffs, native_string parts, uint8 depth, file_mode mode, file** result) {
  error_code err = NO_ERROR;
  fat_file_system* fs = CAST(fat_file_system*, ffs);
  fat_file *parent = NULL, *child = NULL;

  switch (fs->kind) {
    case FAT12_FS:
    case FAT16_FS:
    case FAT32_FS:
      break;

    default:
      return UNIMPL_ERROR;
      break;
  }

  if(ERROR(err = fat_fetch_parent(fs, &parts, depth, &parent)))  {
    return err;
  }

  if(0 == depth) {
    child = parent;
    parent = NULL;
    // Bugged
    // *result = CAST(file*,child);
    // return NO_ERROR;
    // Not bugged
    goto fat_open_file_done;
  }

  // Calling fetch_parent updated the parts
  // up to the name of the file
  err = fat_fetch_file(parent, parts, &child); 
  
  if (ERROR(err) && FNF_ERROR != err) {
    if (NULL != parent) fat_close_file(CAST(file*, parent));
    return err;
  }

  if (ERROR(err) && FNF_ERROR != err) {
  } else if (FNF_ERROR == err || !IS_FOLDER(child->header.type)) {
    // If it is a directory, there is not mode. If the file is not found,
    // the mode must be ignored (we are creating a file)
    switch (mode) {
      case MODE_READ:
      case MODE_READ_WRITE: {
        if (ERROR(err)) break;
        // otherwise everything is ok, there is nothing to
        // do in this mode beside having the cursor at the start.
      } break;

      case MODE_TRUNC:
      case MODE_TRUNC_PLUS: {
        if (ERROR(err)) {
          if (FNF_ERROR == err) {
            // Create the file
            if (ERROR(err = fat_create_file(parts, parent, &child))) {
              break;
            }
          } else {
            break;
          }
        } else {
          if (ERROR(err = fat_truncate_file(child))) {
            break;
          }
        }
      } break;

      case MODE_APPEND:
      case MODE_APPEND_PLUS: {
        if (ERROR(err)) {
          if (FNF_ERROR == err) {
            if (ERROR(err = fat_create_file(parts, parent, &child))) {
              break;
            }
          } else {
            break;
          }
        } else if (ERROR(err = fat_set_to_absolute_position(
                             CAST(file*, child), (child->length > 0) ? (child->length - 1) : 0))) {
          break;
        }

      } break;
      default:
        panic(L"Unhandled file mode");
        break;
    }
  }

fat_open_file_done:
  if (HAS_NO_ERROR(err)) {
    child->header.mode = mode;

    fat_open_chain* link = fat_chain_fetch(fs, child->first_cluster);

    if(NULL == link) {
      link = new_chain_link(fs, child);
      if(NULL == link) return MEM_ERROR;
      fat_chain_add(link);
    }

    link->ref_count++;
    child->link = link;
  }

  if (NULL != parent) fat_close_file(CAST(file*, parent));
  
  fat_set_to_absolute_position(CAST(file*, child), 0);

  *result = CAST(file*, child);

  return err;
}

/* 
Perform the actual deletion of a file on disk. It is important than
this file is not open by any process, since it could result in a file being
overwritten incorrectly.
*/
static error_code fat_actual_remove(fat_file_system* fs, fat_file* f) {
  error_code err = fat_unlink_file(f);
  if(ERROR(err)) return err;

  FAT_directory_entry de;
  // This allows overwriting all the entries correctly
  de.DIR_Name[0] = FAT_UNUSED_ENTRY;

  uint32 old_name_len = kstrlen(f->header.name);

  // Since we delete the file, we allow ourselves this kind of
  // behavior
  f->first_cluster = f->parent.first_cluster;
  f->header.type = TYPE_FOLDER;

  // Correctly locate the DE to overwrite
  fat_reset_cursor(CAST(file*, f));  // might seem useless but it actually
                                     // initialize the cursor correctly

  if (old_name_len > FAT_NAME_LENGTH) {
    // We need to overwrite the old entries
    uint8 no_of_entries = (old_name_len / FAT_CHARS_PER_LONG_NAME_ENTRY) +
                          (old_name_len % FAT_CHARS_PER_LONG_NAME_ENTRY != 0);

    fat_set_to_absolute_position(
        CAST(file*, f),
        f->entry.position - (sizeof(long_file_name_entry) * no_of_entries));

    while (no_of_entries-- > 0) {
      if (ERROR(err =
                    fat_write_file(CAST(file*, f), &de, sizeof(de)))) {
        return err;
      }
    }
  } else {
    fat_set_to_absolute_position(CAST(file*, f), f->entry.position);
  }

  if (ERROR(err = fat_write_file(CAST(file*, f), &de, sizeof(de)))) {
    return err;
  }

  return err;
}

static error_code fat_remove(fs_header* header, file* file) {
  fat_file* f = CAST(fat_file*, file);
  fat_open_chain* link = f->link;

  if(NULL == link) {
    return ARG_ERROR;
  }

  // Indicate that the file is going to be removed
  // when the last reference is closed.
  link->remove_on_close = 1;
  return NO_ERROR;
}

static size_t fat_file_len(file* ff) {
  fat_file* f = CAST(fat_file*, ff);
  return CAST(size_t, f->length);
}

/*
Read the LFN that starts at the cluster "cluster" in the position "position", where
the cluster indicates the start of a directory and the position is the absolute position
relative to that cluster.

The result must be freed by the caller.
*/
static error_code read_lfn(fs_header* fs, uint32 cluster, uint32 position,
                    native_string* result) {
  // We need to find the first entry.
  error_code err = NO_ERROR;
  native_string long_file_name = NULL;
  uint8 j, k, checksum, lfn_entry_count = 0;
  FAT_directory_entry de;
  long_file_name_entry lfn_e;
  fat_file* reader = NULL;

  if (ERROR(err = new_fat_file(&reader))) return err;

  reader->header._fs_header = fs;
  reader->first_cluster = cluster;
  reader->header.type = TYPE_FOLDER;

  fat_reset_cursor(CAST(file*, reader));

  if (ERROR(err =
                fat_set_to_absolute_position(CAST(file*, reader), position))) {
    return err;
  }

  if (ERROR(err = fat_read_file(CAST(file*, reader), &de,
                                sizeof(FAT_directory_entry)))) {
    goto fat_read_lfn_end;
  }

  checksum = lfn_checksum(de.DIR_Name);

  long_file_name = // Todo: add an explanation for this magic number
      CAST(native_char*, kmalloc(sizeof(native_char) * 256));

  if(NULL == long_file_name) {
    return MEM_ERROR;
  }

  j = 0;
  while (HAS_NO_ERROR(err)) {
    // We don't want to go before the file actually starts
    if (position < (sizeof(long_file_name_entry) * (lfn_entry_count + 1))) {
      err = FNF_ERROR;
      goto fat_read_lfn_end;
    }

    uint32 position =
        position - (sizeof(long_file_name_entry) * (lfn_entry_count + 1));

    fat_set_to_absolute_position(CAST(file*, reader), position);

    // We read the LFN entry and scan that it is correct.

    if (ERROR(err = fat_read_file(CAST(file*, reader), &lfn_e,
                                  sizeof(long_file_name_entry)))) {
      goto fat_read_lfn_end;
    }

    if (lfn_e.LDIR_Checksum != checksum) {
      err = FNF_ERROR;
      goto fat_read_lfn_end;
    }

    // The LFN chars are 16 bit UNICODE characters.
    // Since we don't support that (native_string) we
    // discard the top

    for (k = 0; k < 10; k += 2) {
      native_char c = 0xFF & lfn_e.LDIR_Name1[k];
      long_file_name[j++] = (c >= 'a' && c <= 'z') ? (c - 32) : c;
    }

    for (k = 0; k < 12; k += 2) {
      native_char c = 0xFF & lfn_e.LDIR_Name2[k];
      long_file_name[j++] = (c >= 'a' && c <= 'z') ? (c - 32) : c;
    }

    for (k = 0; k < 4; k += 2) {
      native_char c = 0xFF & lfn_e.LDIR_Name3[k];
      long_file_name[j++] = (c >= 'a' && c <= 'z') ? (c - 32) : c;
    }

    lfn_entry_count++;
    if (lfn_e.LDIR_ord & FAT_LAST_LONG_ENTRY) break;
  }

  long_file_name[j] = '\0';

fat_read_lfn_end:
  if (ERROR(err))
    kfree(long_file_name);
  else
    *result = long_file_name;

  fat_close_file(CAST(file*, reader));
  return err;
}

/*
Read the long file name of the file f and place the result
in "result". The result buffer must be freed by the caller
*/
error_code read_lfn(fat_file* f, native_string* result) {
  return read_lfn(f->header._fs_header, f->parent.first_cluster,
                  f->entry.position, result);
}

static dirent* fat_readdir(DIR* dir) {
  fat_file* f = CAST(fat_file*, dir->f);
  fat_file_system* fs = CAST(fat_file_system*, f->header._fs_header);
  error_code err;

  switch (fs->kind) {
    case FAT12_FS:
    case FAT16_FS:
    case FAT32_FS: {
      FAT_directory_entry de;

      while ((err = fat_read_file(CAST(file*, f), &de, sizeof(de))) == sizeof(de)) {
        if (de.DIR_Attr == FAT_ATTR_LONG_NAME) continue;
        if (de.DIR_Name[0] == 0) break;
        if (de.DIR_Name[0] != FAT_UNUSED_ENTRY) {
          if ((de.DIR_Attr & FAT_ATTR_HIDDEN) == 0 &&
              (de.DIR_Attr & FAT_ATTR_VOLUME_ID) == 0) {
            native_string lfn;

            error_code lfn_err =
                read_lfn(f->header._fs_header, f->first_cluster,
                         f->current_pos - sizeof(de), &lfn);

            native_string p1 = dir->ent.d_name;
            if (ERROR(lfn_err)) {
              native_string p2;
              // TODO remove the empty spaces in there
              p1 = copy_without_trailing_spaces(&de.DIR_Name[0], p1, 8);
              *p1++ = '.';
              p2 = p1;
              p1 = copy_without_trailing_spaces(&de.DIR_Name[8], p1, 3);
              if (p1 == p2) p1--;
              *p1++ = '\0';
            } else {
              memcpy(p1, lfn, kstrlen(lfn));
            }

            dir->ent.d_type = (de.DIR_Attr & FAT_ATTR_DIRECTORY)
                                  ? DIR_FILE_TYPE_REG
                                  : DIR_FILE_TYPE_DIR;

            return &dir->ent;
          }
        }
      }

      fat_reset_cursor(dir->f);

      return NULL;
    }
  }

  return NULL;
}


static fat_open_chain* fat_chain_fetch(fat_file_system* fs, uint32 cluster) {
  fat_open_chain* scout = &start_sentinel;

  while(NULL != scout) {
    if(scout->fat_file_first_clus == cluster && scout->fs == fs) {
      break;
    }
    scout = scout->next;
  }

  return scout;
}

/**
 * This lfn checksum algorithm has been taken from the
 * Microsoft specification for the FAT filesystem.
 */
static uint8 lfn_checksum(uint8* short_fname) {
  uint8 sum = 0;
  uint8 i = FAT_NAME_LENGTH;
  uint8* p = short_fname;

  for(; i != 0; i--) {
    sum = ((sum & 1) ? 0x80 : 0) + (sum >> 1) + *p++; 
  }

  return sum;
}

static void name_to_short_file_name(native_string n, short_file_name* result) {
  uint8 full_length = kstrlen(n);
  native_char* p = n;
  uint8 i = 0;

  while (*p != '\0' && *p != '.') {
    if (i < 8) {
      result->name[i] = *p;
    }
    i++;
    p++;
  }

  while (i < 8) result->name[i++] = ' ';

  i = 0;

  if (*p == '.') {
    p++;
    while (*p != '\0') {
      if (i < 3) result->name[8 + i] = *p;
      i++;
      p++;
    }
  }

  while (i < 3) result->name[8 + i++] = ' ';

  if(full_length > FAT_NAME_LENGTH) {
    // TODO: check the folder for collisions
    result->name[6] = '~';
    result->name[7] = '1';
  }
  

  result->name[11] = '\0';
}

static error_code fat_chain_add(fat_open_chain* link) {
  error_code err = NO_ERROR;

  fat_open_chain* old_first = start_sentinel.next;
  start_sentinel.next = link;
  link->prev = &start_sentinel;
  link->next = old_first;
  old_first->prev = link;

  return err;
}

static error_code fat_chain_del(fat_open_chain* link) {
  fat_open_chain* prev = link->prev;
  fat_open_chain* next = link->next;

  prev->next = next;
  next->prev = prev;

  link->next = link->prev = NULL;

  return NO_ERROR;
}

static fat_open_chain* new_chain_link(fat_file_system* fs, fat_file* file) {
  fat_open_chain* nlink = NULL;

  if(NULL == (nlink = CAST(fat_open_chain*, kmalloc(sizeof(fat_open_chain))))) return NULL;

  nlink->fs = fs;
  nlink->ref_count = nlink->remove_on_close = 0;
  nlink->fat_file_first_clus = file->first_cluster;
  nlink->next = nlink->prev = NULL;

  return nlink;
}

static error_code fat_stat(fs_header* header, file* ff, stat_buff* buf) {
  error_code err = NO_ERROR;
  fat_file_system* fs = CAST(fat_file_system*, header);
  fat_file* f = CAST(fat_file*, ff);
  FAT_directory_entry de;

  if(ERROR(err = fat_open_directory_entry(f, &de))) {
    return err;
  }

  uint16 fat_creation_time = as_uint16(de.DIR_CrtTime);
  uint16 fat_modification_time = as_uint16(de.DIR_WrtTime);
  uint16 fat_creation_date = as_uint16(de.DIR_CrtDate);
  uint16 fat_modification_date = as_uint16(de.DIR_WrtDate);
  
  buf->bytes = f->length;
  buf->fs = header;
  buf->fs_block_size = (1 << (fs->_.FAT121632.log2_bps + fs->_.FAT121632.log2_spc));
  buf->type = ff->type;

  uint8 creation_hours, creation_minutes, creation_seconds;
  uint8 modif_hours, modif_minutes, modif_seconds;

  unpack_fat_time(fat_creation_time, &creation_hours, &creation_minutes, &creation_seconds);
  unpack_fat_time(fat_modification_time, &modif_hours, &modif_minutes, &modif_seconds);
  
  int16 creation_years; uint8 creation_month, creation_day;
  int16 modif_year; uint8 modif_month, modif_day;

  unpack_fat_date(fat_creation_date, &creation_years, &creation_month, &creation_day);
  unpack_fat_date(fat_modification_date, &modif_year, &modif_month, &modif_day);
  
  uint32 creation_secs_from_date = days_from_civil(creation_years, creation_month, creation_day) * day_in_sec;
  uint32 modif_secs_from_date = days_from_civil(modif_year, modif_month, modif_day) * day_in_sec;

  // TODO : Get the number of seconds from the Year, month, day
  buf->creation_time_epochs_secs = (creation_hours * hour_in_sec) +
                                   (creation_minutes * min_in_sec) +
                                   creation_seconds + creation_secs_from_date;

  buf->last_modifs_epochs_secs = (modif_hours * hour_in_sec) +
                                 (modif_minutes * min_in_sec) + modif_seconds +
                                 modif_secs_from_date;

  return err;
}

error_code mount_fat(vfnode* parent) {
  start_sentinel.next = &end_sentinel;
  start_sentinel.prev = NULL;

  end_sentinel.prev = &start_sentinel;
  end_sentinel.next = NULL;

  // Init the FS vtable
  _fat_vtable._file_open = fat_file_open;
  _fat_vtable._mkdir = fat_mkdir;
  _fat_vtable._rename = fat_rename;
  _fat_vtable._remove = fat_remove;
  _fat_vtable._stat = fat_stat;

  // Init the file vtable
  _fat_file_vtable._file_close = fat_close_file;
  _fat_file_vtable._file_move_cursor = fat_move_cursor;
  _fat_file_vtable._file_read = fat_read_file;
  _fat_file_vtable._file_set_to_absolute_position = fat_set_to_absolute_position;
  _fat_file_vtable._file_write = fat_write_file;
  _fat_file_vtable._file_len = fat_file_len;
  _fat_file_vtable._readdir = fat_readdir;

  disk_add_all_partitions();
  mount_all_partitions(parent);

  return NO_ERROR;
}

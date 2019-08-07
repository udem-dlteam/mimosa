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

file_vtable _fat_vtable;

error_code new_fat_file(fat_file** result) {
  error_code err = NO_ERROR;
  fat_file* allocated = CAST(fat_file*, kmalloc(sizeof(fat_file)));

  if(NULL == allocated) {
    err = MEM_ERROR;
  } else {
    allocated->header->_vtable = &_fat_vtable;
  }

  *result = allocated;

  return err;
}

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

  if (!S_ISDIR(parent->mode)) {
    return UNKNOWN_ERROR;
  }

  uint32 cluster = parent->first_cluster;
  uint32 position = parent->current_pos;
  while ((err = fat_read_file(parent, &de, sizeof(de))) == sizeof(de)) {
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
          f->mode = S_IFDIR;
        } else {
          f->mode = S_IFREG;
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
  f->mode = S_IFDIR;

  return NO_ERROR;
}

error_code open_root_dir(fat_file_system* fs, fat_file** result) {
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
      f->mode = S_IFDIR;
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

  *result = f;

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

error_code fat_32_find_first_empty_cluster(fat_file_system* fs, uint32* result) {
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


error_code fat_32_set_fat_link_value(fat_file_system* fs, uint32 cluster,
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
  file_set_to_absolute_position(parent_folder, 0);

  uint32 position = parent_folder->current_pos;
  while ((err = fat_read_file(parent_folder, &de, sizeof(de))) == sizeof(de)) {
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
  debug_write("Creating a file at position");
  debug_write(position);
  file_set_to_absolute_position(parent_folder, position);

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

  if (ERROR(err = fat_write_file(parent_folder, &de, sizeof(de)))) {
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
  f->mode = S_IFREG;

  *result = f;

  return err;
}

static error_code create_file(native_char* name, fat_file* parent_folder, fat_file** result) {
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

error_code init_fat() {
  // TODO: init FAT Vtable


  disk_add_all_partitions();
  mount_all_partitions();
  return NO_ERROR;
}
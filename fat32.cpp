#include "fat32.h"
#include "disk.h"
#include "fs.h"
#include "general.h"
#include "rtlib.h"

error_code fat_32_create_empty_file(file_system* fs, file* parent_folder,
                                    native_char* name, file** result) {
  FAT_directory_entry de;
  error_code err;
  native_char normalized_path[NAME_MAX + 1];
  native_string p = normalized_path;
  file* f;

  // Section start is the LBA
  // We reset the file to make sure to find the first position
  file_set_to_absolute_position(parent_folder, 0);

  uint32 position = parent_folder->current_pos;
  while ((err = read_file(parent_folder, &de, sizeof(de))) == sizeof(de)) {
    // This means the entry is available
    if (de.DIR_Name[0] == 0) break;
    // Update the position with the information before
    position = parent_folder->current_pos;
  }

  if(ERROR(err)) {
    panic(L"Not been taken care of yet");
  }

  f = CAST(file*, kmalloc(sizeof(f)));

  if(NULL == f) {
    return MEM_ERROR;
  }

  // We recalculate the position to write the entry
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

  if (ERROR(err = write_file(parent_folder, &de, sizeof(de)))) {
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

error_code 
fat_32_open_root_dir(file_system* fs, file* f) {
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
    mutex_lock(cb->mut);

    BIOS_Parameter_Block* p = CAST(BIOS_Parameter_Block*, cb->buf);
    root_cluster = as_uint32(p->_.FAT32.BPB_RootClus);
    bytes_per_sector = as_uint16(p->BPB_BytsPerSec);
    sectors_per_clusters = p->BPB_SecPerClus;

    mutex_unlock(cb->mut);
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

error_code fat_32_find_first_empty_cluster(file_system* fs, uint32* result) {
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
    mutex_lock(cb->mut);

    for (; i < entries_per_sector; ++i, ++clus) {
      uint32 entry = CAST(uint32*, cb->buf)[i];
      if (found = (entry == 0)) {
        break;
      }
    }

    mutex_unlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
    ++lba;
    i = 0;
  }

  // Could not find an entry: disk out of space
  if (!found) err = DISK_OUT_OF_SPACE;
  *result = clus;
  return err;
}

error_code fat_32_get_fat_link_value(file_system* fs, uint32 cluster,
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
    mutex_lock(cb->mut);

    BIOS_Parameter_Block* p = CAST(BIOS_Parameter_Block*, cb->buf);

    lba = (cluster / entries_per_sector) + as_uint16(p->BPB_RsvdSecCnt);

    mutex_unlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
  }

  uint32 offset = cluster % entries_per_sector;

  {
    if (ERROR(err = disk_cache_block_acquire(d, lba, &cb))) return err;
    mutex_lock(cb->mut);

    *value = *(CAST(uint32*, cb->buf) + offset);

    mutex_unlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
  }

  return err;
}

error_code fat_32_set_fat_link_value(file_system* fs, uint32 cluster,
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
    mutex_lock(cb->mut);

    BIOS_Parameter_Block* p = CAST(BIOS_Parameter_Block*, cb->buf);
    lba = (cluster / entries_per_sector) + as_uint16(p->BPB_RsvdSecCnt);

    mutex_unlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
  }

 
  uint32 offset_in_bytes = (cluster % entries_per_sector) << 2;

  // Read the cache in order to update it
  {  // Very important to lock this write.
    if (ERROR(err = disk_cache_block_acquire(d, lba, &cb))) return err;
    mutex_lock(cb->mut);

    for (int i = 0; i < 4; ++i) {
      cb->buf[i + offset_in_bytes] = as_uint8(value, i);
    }

    cb->dirty = TRUE;
    mutex_unlock(cb->mut);
    if (ERROR(err = disk_cache_block_release(cb))) return err;
  }

  return err;
}

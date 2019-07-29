#include "fat32.h"
#include "fs.h"
#include "disk.h"
#include "general.h"
#include "rtlib.h"

error_code __attribute__((optimize("O0")))
fat_32_create_empty_file(file_system* fs, native_string name, native_string ext,
                         file** result) {
  FAT_directory_entry de;
  error_code err;
  native_char normalized_path[NAME_MAX + 1];
  native_string p = normalized_path;
  file* f;

  //       if (ERROR(err = normalize_path(path, normalized_path))) {
  // #ifdef SHOW_DISK_INFO
  //         term_write(cout, "Failed to normalize the path\n\r");
  // #endif
  //         return err;
  //       }

  if (ERROR(err = fat_32_open_root_dir(fs, f))) {
#ifdef SHOW_DISK_INFO
    term_write(cout, "Error loading the root dir: ");
    term_write(cout, err);
    term_writeline(cout);
#endif
    return err;
  }

  uint32 entry_cluster = f->current_cluster;
  // Section start is the LBA
  uint32 entry_section_start = f->current_section_start;
  uint32 entry_section_pos = f->current_section_pos;
  uint32 section_length = f->current_section_length;

  term_write(cout, "The current root dir section start is:");
  term_write(cout, entry_section_start);

  while ((err = read_file(f, &de, sizeof(de))) == sizeof(de)) {
    // This means the entry is available
    if (de.DIR_Name[0] == 0) break;
    // Update the position with the information before
    entry_cluster = f->current_cluster;
    entry_section_start = f->current_section_start;
    entry_section_pos = f->current_section_pos;
    section_length = f->current_section_length;
  }

  // We got a position for the root entry, we need to find an available FAT
  uint32 first_data_cluster = 2;

  if (ERROR(err = fat_32_find_first_empty_cluster(fs, &first_data_cluster))) {
    return err;
  }

  // Fill with spaces
  for (int i = 0; i < FAT_NAME_LENGTH; ++i) {
    de.DIR_Name[i] = ' ';
  }

  // TODO get the right length to avoid an overrun

  // Copy the name
  memcpy(de.DIR_Name, name, 7);
  memcpy(de.DIR_Name + 8, ext, 3);

  {  // Set the cluster in the descriptor
    uint16 cluster_hi = (first_data_cluster & 0xFFFF0000) >> 16;
    uint16 cluster_lo = (first_data_cluster & 0x0000FFFF);

    for (int i = 0; i < 2; ++i) {
      de.DIR_FstClusHI[i] = as_uint8(cluster_hi, i);
      de.DIR_FstClusLO[i] = as_uint8(cluster_lo, i);
    }
  }

  ide_device* dev = fs->_.FAT121632.d->_.ide.dev;

  term_write(cout, entry_section_start);
  term_writeline(cout);
  term_write(cout, entry_section_pos);
  term_writeline(cout);

  // Write the directory entry
  if(ERROR(err = ide_write(dev, entry_section_start, entry_section_pos, sizeof(de), &de))) {
    return err;
  }

  if(ERROR(err = fat_32_set_fat_link_value(fs, first_data_cluster, FAT_32_EOF))) {
    return err;
  }


  return NO_ERROR;
}

error_code fat_32_open_root_dir(file_system* fs, file* f) {
#ifdef SHOW_DISK_INFO
  term_write(cout, "Loading FAT32 root dir\n\r");
#endif

  BIOS_Parameter_Block* p;
  cache_block* cb;
  disk* d = fs->_.FAT121632.d;
  error_code err;

  if (ERROR(err = disk_cache_block_acquire(d, 0, &cb))) return err;

  p = CAST(BIOS_Parameter_Block*, cb->buf);

  if(ERROR(err = disk_cache_block_release(cb))) return err;

  // debug_write("In open root dir, the FS kind is: ");
  // debug_write(fs->kind);

  f->fs = fs;
  f->current_cluster = as_uint32(p->_.FAT32.BPB_RootClus);

#ifdef SHOW_DISK_INFO
  term_write(cout, "Root cluster is: ");
  term_write(cout, as_uint32(p->_.FAT32.BPB_RootClus));
  term_writeline(cout);
#endif

  f->current_section_start = fs->_.FAT121632.first_data_sector;

  // Length is not there
  f->current_section_length = as_uint16(p->BPB_BytsPerSec) * p->BPB_SecPerClus;

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
}

error_code fat_32_find_first_empty_cluster(file_system* fs, uint32* cluster) {
  BIOS_Parameter_Block* p;
  uint32 lba;
  uint32 offset = 0;
  error_code err;
  disk* d = fs->_.FAT121632.d;
  cache_block* cb;

  if(*cluster < 2) {
    fatal_error("Cannot inspect lower than the second cluster entry");
  }

  if (ERROR(err = disk_cache_block_acquire(d, 0, &cb))) return err;
  p = CAST(BIOS_Parameter_Block*, cb->buf);
  if (ERROR(err = disk_cache_block_release(cb))) return err;

  uint16 cluster_per_sector = (1 << fs->_.FAT121632.log2_bps) >> 2;

  uint32 clus = *cluster;  // We skip the first two cluster
  lba = fs->_.FAT121632.reserved_sectors + (clus / cluster_per_sector);
  uint32 max_sector = as_uint32(p->_.FAT32.BPB_FATSz32) - fs->_.FAT121632.reserved_sectors;

  bool found = FALSE;
  // Inspect all sectors starting from *cluster
  while (lba < max_sector && !found) {
    if (ERROR(err =
                  disk_cache_block_acquire(fs->_.FAT121632.d, lba, &cb)))
      return err;

    uint32 clust_at_start = clus;

    for (; clus < clust_at_start + cluster_per_sector; clus++) {
      uint32 cluster_relative_to_sector = clus % cluster_per_sector;
      uint32 entry = *(CAST(uint32*, cb->buf) + cluster_relative_to_sector);
      found = (entry == 0);
    }

    if (ERROR(err = disk_cache_block_release(cb))) return err;
    ++lba;
  }

  *cluster = clus;
}

error_code fat_32_get_fat_link_value(file_system* fs, uint32 cluster, uint32* value) {
  BIOS_Parameter_Block* p;
  disk* d = fs->_.FAT121632.d;
  error_code err;
  cache_block* cb;
  uint16 cluster_per_sector = (1 << fs->_.FAT121632.log2_bps) >> 2;

  if (cluster < 2) {
    fatal_error("Cannot inspect lower than the second cluster entry");
  }

  if (ERROR(err = disk_cache_block_acquire(d, 0, &cb))) return err;
  p = CAST(BIOS_Parameter_Block*, cb->buf);
  if (ERROR(err = disk_cache_block_release(cb))) return err;

  uint32 lba = (cluster / cluster_per_sector) + as_uint16(p->BPB_RsvdSecCnt);
  uint32 offset = cluster % cluster_per_sector;

  if (ERROR(err = disk_cache_block_acquire(d, lba, &cb))) return err;
  *value= *(CAST(uint32*, cb->buf) + offset);
  if (ERROR(err = disk_cache_block_release(cb))) return err;

  return err;
}

error_code fat_32_set_fat_link_value(file_system* fs, uint32 cluster, uint32 value) {
  BIOS_Parameter_Block* p;
  disk* d = fs->_.FAT121632.d;
  ide_device* dev = d->_.ide.dev;
  error_code err;
  cache_block* cb;
  uint16 cluster_per_sector = (1 << fs->_.FAT121632.log2_bps) >> 2;

  if (cluster < 2) {
    fatal_error("Cannot inspect lower than the second cluster entry");
  }

  if (ERROR(err = disk_cache_block_acquire(d, 0, &cb))) return err;
  p = CAST(BIOS_Parameter_Block*, cb->buf);
  if (ERROR(err = disk_cache_block_release(cb))) return err;

  uint32 lba = (cluster / cluster_per_sector) + as_uint16(p->BPB_RsvdSecCnt);
  uint32 offset_in_bytes = (cluster % cluster_per_sector) << 2;

  uint8 wrt_buff[4];
  
  for(int i = 0; i < 4; ++i) {
    wrt_buff[i] = as_uint8(value, i);
  } 

  ide_write(dev, lba, offset_in_bytes, 4, wrt_buff);
  
  return err;
}
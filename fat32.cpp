#include "fat32.h"
#include "fs.h"
#include "disk.h"
#include "general.h"
#include "rtlib.h"

error_code fat_32_open_root_dir(file_system* fs, file* f, file** result) {
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

  uint32 clus = 2;  // We skip the first two cluster
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
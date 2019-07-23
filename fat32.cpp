#include "fat32.h"
#include "fs.h"
#include "general.h"

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
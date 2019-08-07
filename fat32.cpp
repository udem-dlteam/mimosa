// #include "fat32.h"
// #include "disk.h"
// #include "fs.h"
// #include "general.h"
// #include "rtlib.h"



// error_code fat_32_open_root_dir(file_system* fs, file* f) {
// #ifdef SHOW_DISK_INFO
//   term_write(cout, "Loading FAT32 root dir\n\r");
// #endif

//   cache_block* cb;
//   disk* d = fs->_.FAT121632.d;
//   error_code err;
//   uint16 bytes_per_sector;
//   uint32 root_cluster;
//   uint8 sectors_per_clusters;

//   {
//     if (ERROR(err = disk_cache_block_acquire(d, 0, &cb))) return err;
//     rwmutex_readlock(cb->mut);

//     BIOS_Parameter_Block* p = CAST(BIOS_Parameter_Block*, cb->buf);
//     root_cluster = as_uint32(p->_.FAT32.BPB_RootClus);
//     bytes_per_sector = as_uint16(p->BPB_BytsPerSec);
//     sectors_per_clusters = p->BPB_SecPerClus;

//     rwmutex_readunlock(cb->mut);
//     if (ERROR(err = disk_cache_block_release(cb))) return err;
//   }

//   // debug_write("In open root dir, the FS kind is: ");
//   // debug_write(fs->kind);

//   f->fs = fs;
//   f->first_cluster = f->current_cluster = root_cluster;

// #ifdef SHOW_DISK_INFO
//   term_write(cout, "Root cluster is: ");
//   term_write(cout, root_cluster);
//   term_writeline(cout);
// #endif

//   f->current_section_start = fs->_.FAT121632.first_data_sector;

//   // Length is not there
//   f->current_section_length = bytes_per_sector * sectors_per_clusters;

// #ifdef SHOW_DISK_INFO
//   term_write(cout, "FAT32 ROOT DIR [sector] start=");
//   term_write(cout, fs->_.FAT121632.first_data_sector);
// #endif
//   f->current_section_pos = 0;
//   f->current_pos = 0;
//   // Since the FAT32 root directory has no fixed size, we don't specify a length
//   // (it would be slow to calculate it everytime...). On a directory, the length
//   // is not used anyways when reading the file. We simply read until EOF.
//   f->length = 0;
//   f->mode = S_IFDIR;

//   return NO_ERROR;
// }




// file: "disk.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 01 Nov 01  initial version (Marc Feeley)

#ifndef __DISK_H
#define __DISK_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "ide.h"

//-----------------------------------------------------------------------------

//
// Definitions for disk drives and partitions.
//

#define DISK_IDE 0
#define MAX_NB_DISKS 32
#define DISK_LOG2_BLOCK_SIZE 9

void cache_block_maid_run();

const struct {
  uint8 type;
  native_string name;
  bool lba;
} partition_type_table[] = {{0x01, "Primary DOS 12-bit FAT", false},
                            {0x02, "Xenix / file system", false},
                            {0x03, "Xenix /usr file system", false},
                            {0x04, "Primary DOS 16-bit FAT", false},
                            {0x05, "Extended DOS", false},
                            {0x06, "Primary big DOS >32Mb", false},
                            {0x07, "OS/2 HPFS, NTFS, QNX or Advanced Unix", false},
                            {0x08, "AIX boot partition", false},
                            {0x09, "AIX file system partition or Coherent", false},
                            {0x0A, "OS/2 Boot Manager or Coherent", false},
                            {0x0B, "DOS or Windows 95 with 32-bit FAT", false},
                            {0x0C, "DOS or Windows 95 with 32-bit FAT, LBA", true},
                            {0x0E, "Primary big DOS >32Mb LBA", true},
                            {0x0F, "Extended DOS, LBA", true},
                            {0x10, "OPUS", false},
                            {0x11, "DOS 12-bit FAT Hidden Partition", false},
                            {0x12, "Compaq Configuration Partition", false},
                            {0x14, "DOS 16-bit FAT <32Mb Hidden", false},
                            {0x16, "DOS 16-bit FAT >=32Mb Hidden", false},
                            {0x17, "OS/2 HPFS Hidden", false},
                            {0x18, "AST Windows swap file", false},
                            {0x19, "Willowtech Photon coS", false},
                            {0x1B, "WIN95 OSR2 32-bit FAT Hidden", false},
                            {0x1C, "WIN95 OSR2 32-bit FAT, LBA, Hidden", true},
                            {0x1E, "FAT95 Hidden", false},
                            {0x20, "Willowsoft Overture File system", false},
                            {0x21, "FSo2 Oxygen File system", false},
                            {0x22, "Extended Oxygen File system", false},
                            {0x24, "NEC DOS 3.x", false},
                            {0x38, "THEOS ver 3.2 2Gb Partition", false},
                            {0x39, "THEOS ver 4 Spanned Partition", false},
                            {0x3A, "THEOS ver 4 4Gb Partition", false},
                            {0x3B, "THEOS ver 4 Extended Partition", false},
                            {0x3C, "Partition magic Recovery Partition", false},
                            {0x40, "VENIX 286", false},
                            {0x42, "SFS (Secure File System)", false},
                            {0x50, "Disk manager", false},
                            {0x51, "Disk manager", false},
                            {0x52, "CP/M or Microport SysV/AT", false},
                            {0x56, "GoldenBow VFeature", false},
                            {0x61, "Speedstor", false},
                            {0x63, "ISC Unix, System V/386, GNU HURD or Mach", false},
                            {0x64, "Novell Netware 2.xx", false},
                            {0x65, "Novell Netware 3.xx", false},
                            {0x70, "DiskSecure Multi-Boot", false},
                            {0x75, "IBM PCIX", false},
                            {0x80, "Minix 1.1 -> 1.4a", false},
                            {0x81, "Minix 1.4b -> 1.5.10", false},
                            {0x82, "Linux Swap", false},
                            {0x83, "Linux File system", false},
                            {0x84, "OS/2 type 04 hidden DOS C:", false},
                            {0x93, "Amoeba File system", false},
                            {0x94, "Amoeba Bad Block Table", false},
                            {0xA5, "FreeBSD/NetBSD/386BSD", false},
                            {0xA6, "OpenBSD", false},
                            {0xA7, "NEXTSTEP", false},
                            {0xB7, "BSDI BSD/386 File system", false},
                            {0xB8, "BSDI BSD/386 swap", false},
                            {0xC1, "DR-DOS 6.0 secured 12-bit FAT partition", false},
                            {0xC4, "DR-DOS 6.0 secured 16-bit FAT partition", false},
                            {0xC6, "DR-DOS 6.0 secured Huge partition", false},
                            {0xC7, "Syrinx", false},
                            {0xDB, "Concurrent CPM, C.DOS, CTOS", false},
                            {0xE1, "Speed", false},
                            {0xE3, "Speed", false},
                            {0xE4, "Speed", false},
                            {0xF1, "Speed", false},
                            {0xF2, "DOS 3.3+ Secondary", false},
                            {0xF4, "Speed", false},
                            {0xFE, "LANstep", false},
                            {0xFF, "Xenix Bad Block Table", false}};

typedef struct disk_struct
  {
    uint8 id; // 0 to MAX_NB_DISKS-1
    uint8 kind; // DISK_IDE is currently the only possibility

    uint8 log2_sector_size;
    uint8 partition_type;
    uint32 partition_path;
    uint32 partition_start; // in sectors
    uint32 partition_length; // in sectors

    union
      {
        struct
          {
            ide_device* dev;
          } ide;
      } _;
  } disk;

typedef struct cache_block_deq
  {
    struct cache_block_deq* next;
    struct cache_block_deq* prev;
  } cache_block_deq;

typedef struct cache_block_struct
  {
    cache_block_deq LRU_deq;
    cache_block_deq hash_bucket_deq;
    disk* d;
    uint32 sector_pos;
    uint32 refcount;
    rwmutex* mut;
    condvar* cv;
    error_code err;
    uint8 buf[1 << DISK_LOG2_BLOCK_SIZE];
    uint8 dirty : 1;
  } cache_block;

disk* disk_alloc ();

disk* disk_find (uint32 index);

uint32 disk_BIOS_CHS_to_LBA (disk* d, uint8 BIOS_CHS[3]);
uint32 disk_max_BIOS_CHS_to_LBA (disk* d);

void disk_print_id (disk* d);

void disk_add_all_partitions ();

error_code disk_read_sectors
  (disk* d,
   uint32 sector_pos,
   void* buf,
   uint32 count);

error_code disk_write_sectors(disk* d, uint32 sector_pos, void* sector_buff,
                              uint32 sector_count);

error_code disk_cache_block_acquire
  (disk* d,
   uint32 sector_pos,
   cache_block** block);

error_code disk_cache_block_release (cache_block* block);

void setup_disk ();

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //

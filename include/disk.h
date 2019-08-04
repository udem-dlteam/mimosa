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
} partition_type_table[] = {{0x01, "Primary DOS 12-bit FAT", FALSE},
                            {0x02, "Xenix / file system", FALSE},
                            {0x03, "Xenix /usr file system", FALSE},
                            {0x04, "Primary DOS 16-bit FAT", FALSE},
                            {0x05, "Extended DOS", FALSE},
                            {0x06, "Primary big DOS >32Mb", FALSE},
                            {0x07, "OS/2 HPFS, NTFS, QNX or Advanced Unix", FALSE},
                            {0x08, "AIX boot partition", FALSE},
                            {0x09, "AIX file system partition or Coherent", FALSE},
                            {0x0A, "OS/2 Boot Manager or Coherent", FALSE},
                            {0x0B, "DOS or Windows 95 with 32-bit FAT", FALSE},
                            {0x0C, "DOS or Windows 95 with 32-bit FAT, LBA", TRUE},
                            {0x0E, "Primary big DOS >32Mb LBA", TRUE},
                            {0x0F, "Extended DOS, LBA", TRUE},
                            {0x10, "OPUS", FALSE},
                            {0x11, "DOS 12-bit FAT Hidden Partition", FALSE},
                            {0x12, "Compaq Configuration Partition", FALSE},
                            {0x14, "DOS 16-bit FAT <32Mb Hidden", FALSE},
                            {0x16, "DOS 16-bit FAT >=32Mb Hidden", FALSE},
                            {0x17, "OS/2 HPFS Hidden", FALSE},
                            {0x18, "AST Windows swap file", FALSE},
                            {0x19, "Willowtech Photon coS", FALSE},
                            {0x1B, "WIN95 OSR2 32-bit FAT Hidden", FALSE},
                            {0x1C, "WIN95 OSR2 32-bit FAT, LBA, Hidden", TRUE},
                            {0x1E, "FAT95 Hidden", FALSE},
                            {0x20, "Willowsoft Overture File system", FALSE},
                            {0x21, "FSo2 Oxygen File system", FALSE},
                            {0x22, "Extended Oxygen File system", FALSE},
                            {0x24, "NEC DOS 3.x", FALSE},
                            {0x38, "THEOS ver 3.2 2Gb Partition", FALSE},
                            {0x39, "THEOS ver 4 Spanned Partition", FALSE},
                            {0x3A, "THEOS ver 4 4Gb Partition", FALSE},
                            {0x3B, "THEOS ver 4 Extended Partition", FALSE},
                            {0x3C, "Partition magic Recovery Partition", FALSE},
                            {0x40, "VENIX 286", FALSE},
                            {0x42, "SFS (Secure File System)", FALSE},
                            {0x50, "Disk manager", FALSE},
                            {0x51, "Disk manager", FALSE},
                            {0x52, "CP/M or Microport SysV/AT", FALSE},
                            {0x56, "GoldenBow VFeature", FALSE},
                            {0x61, "Speedstor", FALSE},
                            {0x63, "ISC Unix, System V/386, GNU HURD or Mach", FALSE},
                            {0x64, "Novell Netware 2.xx", FALSE},
                            {0x65, "Novell Netware 3.xx", FALSE},
                            {0x70, "DiskSecure Multi-Boot", FALSE},
                            {0x75, "IBM PCIX", FALSE},
                            {0x80, "Minix 1.1 -> 1.4a", FALSE},
                            {0x81, "Minix 1.4b -> 1.5.10", FALSE},
                            {0x82, "Linux Swap", FALSE},
                            {0x83, "Linux File system", FALSE},
                            {0x84, "OS/2 type 04 hidden DOS C:", FALSE},
                            {0x93, "Amoeba File system", FALSE},
                            {0x94, "Amoeba Bad Block Table", FALSE},
                            {0xA5, "FreeBSD/NetBSD/386BSD", FALSE},
                            {0xA6, "OpenBSD", FALSE},
                            {0xA7, "NEXTSTEP", FALSE},
                            {0xB7, "BSDI BSD/386 File system", FALSE},
                            {0xB8, "BSDI BSD/386 swap", FALSE},
                            {0xC1, "DR-DOS 6.0 secured 12-bit FAT partition", FALSE},
                            {0xC4, "DR-DOS 6.0 secured 16-bit FAT partition", FALSE},
                            {0xC6, "DR-DOS 6.0 secured Huge partition", FALSE},
                            {0xC7, "Syrinx", FALSE},
                            {0xDB, "Concurrent CPM, C.DOS, CTOS", FALSE},
                            {0xE1, "Speed", FALSE},
                            {0xE3, "Speed", FALSE},
                            {0xE4, "Speed", FALSE},
                            {0xF1, "Speed", FALSE},
                            {0xF2, "DOS 3.3+ Secondary", FALSE},
                            {0xF4, "Speed", FALSE},
                            {0xFE, "LANstep", FALSE},
                            {0xFF, "Xenix Bad Block Table", FALSE}};

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
    mutex* mut;
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
// mode: C     //
// End: //

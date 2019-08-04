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

extern const struct partition_type {
  uint8 type;
  native_string name;
  bool lba;
} partition_type_table[];

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

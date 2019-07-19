// file: "disk.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 23 nov 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "disk.h"
#include "ide.h"
#include "rtlib.h"
#include "term.h"

//-----------------------------------------------------------------------------

#define DISK_CACHE_SIZE 100
#define CACHE_BLOCK_HASH_TABLE_SIZE 5059

typedef struct disk_module_struct {
  disk disk_table[MAX_NB_DISKS];
  uint32 nb_disks;
  mutex* cache_mut;
  condvar* cache_cv;
  cache_block_deq LRU_deq;
  cache_block_deq cache_block_hash_table[CACHE_BLOCK_HASH_TABLE_SIZE];
} disk_module;

static disk_module disk_mod;

disk* disk_alloc() {
  if (disk_mod.nb_disks < MAX_NB_DISKS)
    return &disk_mod.disk_table[disk_mod.nb_disks++];
  return NULL;
}

disk* disk_find(uint32 index) {
  if (index < disk_mod.nb_disks) return &disk_mod.disk_table[index];
  return NULL;
}

uint32 disk_BIOS_CHS_to_LBA(disk* d, uint8 BIOS_CHS[3]) {
  uint16 C = ((CAST(uint16, BIOS_CHS[1]) & 0xc0) << 2) + BIOS_CHS[2];
  uint8 H = BIOS_CHS[0];
  uint8 S = BIOS_CHS[1] & 0x3f;
  uint32 shift = 0;

  while ((d->_.ide.dev->cylinders_per_disk >> shift) > 1024 && shift < 4)
    shift++;

  return (((CAST(uint32, (C << shift)) * d->_.ide.dev->heads_per_cylinder) +
           H) *
          d->_.ide.dev->sectors_per_track) +
         S - 1;
}

uint32 disk_max_BIOS_CHS_to_LBA(disk* d) {
  uint32 cyls;
  uint32 shift = 0;

  while ((d->_.ide.dev->cylinders_per_disk >> shift) > 1024 && shift < 4)
    shift++;

  cyls = d->_.ide.dev->cylinders_per_disk >> shift;

  if (cyls > 1024) cyls = 1024;

  return ((cyls * d->_.ide.dev->heads_per_cylinder *
           d->_.ide.dev->sectors_per_track)
          << shift) -
         1;
}

error_code disk_read_sectors(disk* d, uint32 sector_pos, void* buf,
                             uint32 count) {
  if (sector_pos < d->partition_length &&
      sector_pos + count <= d->partition_length) {
    switch (d->kind) {
      case DISK_IDE:
        return ide_read_sectors(d->_.ide.dev, d->partition_start + sector_pos,
                                buf, count);

      default:
        return UNKNOWN_ERROR;
    }
  }

  return UNKNOWN_ERROR;
}

error_code disk_cache_block_acquire(disk* d, uint32 sector_pos,
                                    cache_block** block) {
  error_code err;
  cache_block* cb = NULL;
  cache_block_deq* LRU_deq;
  cache_block_deq* LRU_probe;
  cache_block_deq* hash_bucket_deq;
  cache_block_deq* hash_bucket_probe;

again:

  disk_mod.cache_mut->lock();

  for (;;) {
    LRU_deq = &disk_mod.LRU_deq;

    hash_bucket_deq =
        &disk_mod
             .cache_block_hash_table[sector_pos % CACHE_BLOCK_HASH_TABLE_SIZE];

    hash_bucket_probe = hash_bucket_deq->next;

    while (hash_bucket_probe != hash_bucket_deq) {
      cb = CAST(cache_block*,
                CAST(uint8*, hash_bucket_probe) -
                    (CAST(uint8*, &cb->hash_bucket_deq) - CAST(uint8*, cb)));
      if (cb->d == d && cb->sector_pos == sector_pos) break;
      hash_bucket_probe = hash_bucket_probe->next;
    }

    if (hash_bucket_probe != hash_bucket_deq) {
      cache_block_deq* n;
      cache_block_deq* p;

      n = cb->hash_bucket_deq.next;
      p = cb->hash_bucket_deq.prev;
      n->prev = p;
      p->next = n;
      hash_bucket_deq->next->prev = &cb->hash_bucket_deq;
      cb->hash_bucket_deq.next = hash_bucket_deq->next;
      hash_bucket_deq->next = &cb->hash_bucket_deq;
      cb->hash_bucket_deq.prev = hash_bucket_deq;

      n = cb->LRU_deq.next;
      p = cb->LRU_deq.prev;
      n->prev = p;
      p->next = n;
      LRU_deq->next->prev = &cb->LRU_deq;
      cb->LRU_deq.next = LRU_deq->next;
      LRU_deq->next = &cb->LRU_deq;
      cb->LRU_deq.prev = LRU_deq;

      cb->refcount++;
      cb->mut->lock();
      disk_mod.cache_mut->unlock();
      while ((err = cb->err) == IN_PROGRESS) cb->cv->wait(cb->mut);
      cb->mut->unlock();
      cb->cv->signal();

      if (ERROR(err)) {
        disk_cache_block_release(cb);  // ignore error
        goto again;
      }

      break;
    }

    LRU_probe = LRU_deq->prev;

    while (LRU_probe != &disk_mod.LRU_deq) {
      cb = CAST(cache_block*,
                CAST(uint8*, LRU_probe) -
                    (CAST(uint8*, &cb->LRU_deq) - CAST(uint8*, cb)));
      if (cb->refcount == 0) break;
      LRU_probe = LRU_probe->prev;
    }

    if (LRU_probe != LRU_deq) {
      cache_block_deq* n;
      cache_block_deq* p;

      n = cb->hash_bucket_deq.next;
      p = cb->hash_bucket_deq.prev;
      n->prev = p;
      p->next = n;
      hash_bucket_deq->next->prev = &cb->hash_bucket_deq;
      cb->hash_bucket_deq.next = hash_bucket_deq->next;
      hash_bucket_deq->next = &cb->hash_bucket_deq;
      cb->hash_bucket_deq.prev = hash_bucket_deq;

      n = cb->LRU_deq.next;
      p = cb->LRU_deq.prev;
      n->prev = p;
      p->next = n;
      LRU_deq->next->prev = &cb->LRU_deq;
      cb->LRU_deq.next = LRU_deq->next;
      LRU_deq->next = &cb->LRU_deq;
      cb->LRU_deq.prev = LRU_deq;

      cb->refcount = 1;
      cb->d = d;
      cb->sector_pos = sector_pos;
      cb->mut->lock();
      disk_mod.cache_mut->unlock();
      cb->err = IN_PROGRESS;
      err =
          disk_read_sectors(d, sector_pos, cb->buf,
                            1 << (DISK_LOG2_BLOCK_SIZE - d->log2_sector_size));
      cb->err = err;
      cb->mut->unlock();
      cb->cv->signal();

      if (ERROR(err)) {
        disk_mod.cache_mut->lock();
        cb->d = NULL;  // so that this cache block can't be found again
        disk_mod.cache_mut->unlock();
        disk_cache_block_release(cb);  // ignore error
        return err;
      }

      break;
    }

    disk_mod.cache_cv->wait(disk_mod.cache_mut);
  }

  *block = cb;

  return NO_ERROR;
}

error_code disk_cache_block_release(cache_block* block) {
  uint32 n;

  disk_mod.cache_mut->lock();
  n = --block->refcount;
  disk_mod.cache_mut->unlock();

  if (n == 0) disk_mod.cache_cv->signal();

  return NO_ERROR;
}

//-----------------------------------------------------------------------------

// Table of partition types, for debugging.

static const struct {
  uint8 type;
  native_string name;
} partition_type_table[] = {{0x01, "Primary DOS 12-bit FAT"},
                            {0x02, "Xenix / file system"},
                            {0x03, "Xenix /usr file system"},
                            {0x04, "Primary DOS 16-bit FAT"},
                            {0x05, "Extended DOS"},
                            {0x06, "Primary big DOS >32Mb"},
                            {0x07, "OS/2 HPFS, NTFS, QNX or Advanced Unix"},
                            {0x08, "AIX boot partition"},
                            {0x09, "AIX file system partition or Coherent"},
                            {0x0A, "OS/2 Boot Manager or Coherent"},
                            {0x0B, "DOS or Windows 95 with 32-bit FAT"},
                            {0x0C, "DOS or Windows 95 with 32-bit FAT, LBA"},
                            {0x0E, "Primary big DOS >32Mb LBA"},
                            {0x0F, "Extended DOS, LBA"},
                            {0x10, "OPUS"},
                            {0x11, "DOS 12-bit FAT Hidden Partition"},
                            {0x12, "Compaq Configuration Partition"},
                            {0x14, "DOS 16-bit FAT <32Mb Hidden"},
                            {0x16, "DOS 16-bit FAT >=32Mb Hidden"},
                            {0x17, "OS/2 HPFS Hidden"},
                            {0x18, "AST Windows swap file"},
                            {0x19, "Willowtech Photon coS"},
                            {0x1B, "WIN95 OSR2 32-bit FAT Hidden"},
                            {0x1C, "WIN95 OSR2 32-bit FAT, LBA, Hidden"},
                            {0x1E, "FAT95 Hidden"},
                            {0x20, "Willowsoft Overture File system"},
                            {0x21, "FSo2 Oxygen File system"},
                            {0x22, "Extended Oxygen File system"},
                            {0x24, "NEC DOS 3.x"},
                            {0x38, "THEOS ver 3.2 2Gb Partition"},
                            {0x39, "THEOS ver 4 Spanned Partition"},
                            {0x3A, "THEOS ver 4 4Gb Partition"},
                            {0x3B, "THEOS ver 4 Extended Partition"},
                            {0x3C, "Partition magic Recovery Partition"},
                            {0x40, "VENIX 286"},
                            {0x42, "SFS (Secure File System)"},
                            {0x50, "Disk manager"},
                            {0x51, "Disk manager"},
                            {0x52, "CP/M or Microport SysV/AT"},
                            {0x56, "GoldenBow VFeature"},
                            {0x61, "Speedstor"},
                            {0x63, "ISC Unix, System V/386, GNU HURD or Mach"},
                            {0x64, "Novell Netware 2.xx"},
                            {0x65, "Novell Netware 3.xx"},
                            {0x70, "DiskSecure Multi-Boot"},
                            {0x75, "IBM PCIX"},
                            {0x80, "Minix 1.1 -> 1.4a"},
                            {0x81, "Minix 1.4b -> 1.5.10"},
                            {0x82, "Linux Swap"},
                            {0x83, "Linux File system"},
                            {0x84, "OS/2 type 04 hidden DOS C:"},
                            {0x93, "Amoeba File system"},
                            {0x94, "Amoeba Bad Block Table"},
                            {0xA5, "FreeBSD/NetBSD/386BSD"},
                            {0xA6, "OpenBSD"},
                            {0xA7, "NEXTSTEP"},
                            {0xB7, "BSDI BSD/386 File system"},
                            {0xB8, "BSDI BSD/386 swap"},
                            {0xC1, "DR-DOS 6.0 secured 12-bit FAT partition"},
                            {0xC4, "DR-DOS 6.0 secured 16-bit FAT partition"},
                            {0xC6, "DR-DOS 6.0 secured Huge partition"},
                            {0xC7, "Syrinx"},
                            {0xDB, "Concurrent CPM, C.DOS, CTOS"},
                            {0xE1, "Speed"},
                            {0xE3, "Speed"},
                            {0xE4, "Speed"},
                            {0xF1, "Speed"},
                            {0xF2, "DOS 3.3+ Secondary"},
                            {0xF4, "Speed"},
                            {0xFE, "LANstep"},
                            {0xFF, "Xenix Bad Block Table"}};

static native_string partition_name_from_type(uint8 type) {
  uint32 i;

  for (i = 0;
       i < sizeof(partition_type_table) / sizeof(partition_type_table[0]); i++)
    if (partition_type_table[i].type == type)
      return partition_type_table[i].name;

  return "Unknown";
}

static void disk_print_path(uint32 path) {
  if (path > 0) {
    disk_print_path(path / 10);
    term_write(term_write(cout, "/"), (path % 10) - 1);
  }
}

void disk_print_id(disk* d) {
  if (d->_.ide.dev->kind == IDE_DEVICE_ATA) {
    term_write(cout, "ATA");
  } else {
    term_write(cout, "ATAPI");
  }

  term_write(cout, " ide");
  term_write(cout, d->_.ide.dev->ctrl->id);
  term_write(cout, ".");
  term_write(cout, d->_.ide.dev->id);

  disk_print_path(d->partition_path);

  term_write(cout, " ");
  term_write(cout, (d->partition_length >> (20 - d->log2_sector_size)));
  term_write(cout, "MB");

  if (d->partition_type != 0) {
    term_write(cout, " (");
    term_write(cout, partition_name_from_type(d->partition_type));
    term_write(cout, ")");
  }
}

//-----------------------------------------------------------------------------

// Layout of a Master Boot Record (MBR) which contains a disk's
// partition table.

typedef struct partition_table_entry_struct {
  uint8 active_flag;
  uint8 start_BIOS_CHS[3];
  uint8 type;
  uint8 end_BIOS_CHS[3];
  uint8 start_LBA[4];
  uint8 nb_sectors[4];
} partition_table_entry;

typedef struct Master_Boot_Record_struct {
  uint8 pad[0x1be];
  partition_table_entry partition_table[4];
  uint8 signature[2];
} Master_Boot_Record;

//-----------------------------------------------------------------------------

void disk_add_all_partitions() {
  uint32 index = 0;
  disk* d;

  while ((d = disk_find(index)) != NULL) {
    if (d->partition_type == 0      // whole disk
        || d->partition_type == 5)  // extended partition
    {
      Master_Boot_Record mbr;
      uint32 i;
      uint32 max_LBA_when_using_BIOS_CHS = disk_max_BIOS_CHS_to_LBA(d);
      disk* part;

      if (!ERROR(disk_read_sectors(d, 0, &mbr, 1))) {
        for (i = 0; i < 4; i++) {
          partition_table_entry* p = &mbr.partition_table[i];
          uint8 type;
          uint32 start_LBA;
          uint32 end_LBA;
          uint32 nb_sectors;

          type = p->type;

          if (type == 0) continue;

          start_LBA = d->partition_start + as_uint32(p->start_LBA);
          nb_sectors = as_uint32(p->nb_sectors);
          end_LBA = start_LBA + nb_sectors - 1;

          if ((p->active_flag & 0x7f) != 0) {
#ifdef SHOW_DISK_INFO
            term_write(cout, "*** incorrect active_flag\n");
#endif
            continue;
          }

          if (((start_LBA < max_LBA_when_using_BIOS_CHS)
                   ? start_LBA
                   : max_LBA_when_using_BIOS_CHS) !=
              disk_BIOS_CHS_to_LBA(d, p->start_BIOS_CHS)) {
#ifdef SHOW_DISK_INFO
            term_write(
                cout,
                "*** start CHS inconsistent with start_LBA (start CHS = ");
            term_write(cout, disk_BIOS_CHS_to_LBA(d, p->start_BIOS_CHS));
            term_write(cout, " start_LBA = ");
            term_write(cout, start_LBA);
            term_write(cout, ")\n");
#endif
            continue;
          }

          if (((end_LBA < max_LBA_when_using_BIOS_CHS)
                   ? end_LBA
                   : max_LBA_when_using_BIOS_CHS) !=
              disk_BIOS_CHS_to_LBA(d, p->end_BIOS_CHS)) {
#ifdef SHOW_DISK_INFO
            term_write(cout,
                       "*** end CHS inconsistent with nb_sectors (end CHS =");
            term_write(cout, disk_BIOS_CHS_to_LBA(d, p->end_BIOS_CHS));
            term_write(cout, " end_LBA = ");
            term_write(cout, end_LBA);
            term_write(cout, ")\n");
#endif
            continue;
          }

          if (start_LBA < d->partition_start ||
              end_LBA >= d->partition_start + d->partition_length) {
#ifdef SHOW_DISK_INFO
            term_write(
                cout,
                "*** partition exceeds disk or containing partition bounds\n");
#endif
            continue;
          }

          part = disk_alloc();

          if (part == NULL) continue;

          /////something like this would be better but bombs: *part = *d;

          part->kind = d->kind;
          part->log2_sector_size = d->log2_sector_size;
          part->partition_type = type;
          part->partition_path = d->partition_path * 10 + i + 1;
          part->partition_start = start_LBA;
          part->partition_length = nb_sectors;
          part->_.ide.dev = d->_.ide.dev;
        }
      }
    }
    index++;
  }
}

void setup_disk() {
  uint32 i;

  disk_mod.nb_disks = 0;

  for (i = 0; i < MAX_NB_DISKS; i++) disk_mod.disk_table[i].id = i;

  disk_mod.cache_mut = new mutex;
  disk_mod.cache_cv = new condvar;

  disk_mod.LRU_deq.next = &disk_mod.LRU_deq;
  disk_mod.LRU_deq.prev = &disk_mod.LRU_deq;

  for (i = 0; i < CACHE_BLOCK_HASH_TABLE_SIZE; i++) {
    cache_block_deq* deq = &disk_mod.cache_block_hash_table[i];
    deq->next = deq;
    deq->prev = deq;
  }

  for (i = 0; i < DISK_CACHE_SIZE; i++) {
    cache_block_deq* LRU_deq;
    cache_block_deq* hash_bucket_deq;
    cache_block* cb = CAST(cache_block*, kmalloc(sizeof(cache_block)));

    if (cb == NULL) fatal_error("can't allocate disk cache");

    LRU_deq = &cb->LRU_deq;
    hash_bucket_deq = &cb->hash_bucket_deq;

    LRU_deq->next = &disk_mod.LRU_deq;
    LRU_deq->prev = disk_mod.LRU_deq.prev;
    disk_mod.LRU_deq.prev->next = LRU_deq;
    disk_mod.LRU_deq.prev = LRU_deq;

    hash_bucket_deq->next = hash_bucket_deq;
    hash_bucket_deq->prev = hash_bucket_deq;

    // cb->d and cb->sector_pos and cb->err can stay undefined

    cb->refcount = 0;
    cb->mut = new mutex;
    cb->cv = new condvar;
  }
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //

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

error_code disk_write_sectors(disk* d, uint32 sector_pos, void* sector_buffs, uint32 sector_count) {
  
    error_code err = UNKNOWN_ERROR;

    if (sector_pos < d->partition_length && sector_pos + sector_count <= d->partition_length) {
    switch (d->kind) {
      case DISK_IDE:
        return ide_write_sectors(d->_.ide.dev, d->partition_start + sector_pos,
                                 sector_buffs, sector_count);
      default:
        return UNIMPL_ERROR;
    }
  }

  return err;
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

  mutex_lock(disk_mod.cache_mut);

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
      mutex_lock(cb->mut);
      mutex_unlock(disk_mod.cache_mut);
      while ((err = cb->err) == IN_PROGRESS) {
        condvar_wait(cb->cv, cb->mut);
      }

      mutex_unlock(cb->mut);
      condvar_signal(cb->cv);

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

#ifdef USE_BLOCK_REF_COUNTER_FREE
      if (cb->refcount == 0) break;
#else
      // If we dont use the reference counting,
      // we cannot allocate dirty blocks
      if ((cb->refcount == 0) && (!cb->dirty)) break;
#endif

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
      mutex_lock(cb->mut);
      mutex_unlock(disk_mod.cache_mut);
      cb->err = IN_PROGRESS;
      err =
          disk_read_sectors(d, sector_pos, cb->buf,
                            1 << (DISK_LOG2_BLOCK_SIZE - d->log2_sector_size));
      cb->err = err;
      mutex_unlock(cb->mut);
      condvar_signal(cb->cv);

      if (ERROR(err)) {
        mutex_lock(disk_mod.cache_mut);
        cb->d = NULL;  // so that this cache block can't be found again
        mutex_unlock(disk_mod.cache_mut);
        disk_cache_block_release(cb);  // ignore error
        return err;
      }

      break;
    }

    condvar_wait(disk_mod.cache_cv, disk_mod.cache_mut);
  }

  *block = cb;

  return NO_ERROR;
}

static error_code flush_block(cache_block* block, time timeout) {
  error_code err = NO_ERROR;

  if (!block->dirty) {
    return err;
  }

  if (mutex_lock_or_timeout(block->mut, timeout)) {
    // Make sure it hasn't been cleaned in the wait
    if (block->dirty) {
      if (HAS_NO_ERROR(err = disk_write_sectors(block->d, block->sector_pos,
                                                block->buf, 1))) {
        block->dirty = 0;
        err = 1;  // We flushed a single block
      }
    }

    mutex_unlock(block->mut);
  }

  return err;
}

error_code disk_cache_block_release(cache_block* block) {
  error_code err = NO_ERROR;
  uint32 n;

#ifdef USE_BLOCK_REF_COUNTER_FREE
  if (block->refcount == 1) {
    // we are the last reference to this block
    // this means we don't need any lock on it.
    // If the cache block maid is activated, we
    // will never have to wait on a lock, since
    // we are the last reference to it.
    // However, it is possible that we wait because
    // of the maid.

    flush_block(block, seconds_to_time(0));
  }
#endif

  if(HAS_NO_ERROR(err)) {
    mutex_lock(disk_mod.cache_mut);
    n = --block->refcount;
    mutex_unlock(disk_mod.cache_mut);

    if (n == 0) {
      condvar_signal(disk_mod.cache_cv);
    }
  }

  return err;
}

//-----------------------------------------------------------------------------

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

          bool only_lba = partition_type_table[type].lba;

          start_LBA = d->partition_start + as_uint32(p->start_LBA);
          nb_sectors = as_uint32(p->nb_sectors);
          end_LBA = start_LBA + nb_sectors - 1;

          if ((p->active_flag & 0x7f) != 0) {
#ifdef SHOW_DISK_INFO
            term_write(cout, "*** incorrect active_flag\n");
#endif
            continue;
          }

          if (!only_lba) {
            // CHS fields for LBA partitions are possibly erronous and should not be read
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

  disk_mod.cache_mut = new_mutex(CAST(mutex*, kmalloc(sizeof(mutex))));
  disk_mod.cache_cv = new_condvar(CAST(condvar*, kmalloc(sizeof(condvar))));

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

    if (cb == NULL) panic(L"can't allocate disk cache");

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
    cb->mut = new_mutex(CAST(mutex*, kmalloc(sizeof(mutex))));
    cb->cv = new_condvar(CAST(condvar*, kmalloc(sizeof(condvar))));
  }
}

//-----------------------------------------------------------------------------
// Cache block cleaning thread
//-----------------------------------------------------------------------------

void cache_block_maid_run() {
  cache_block* cb;
  cache_block_deq* deq;
  cache_block_deq* lru_probe;
  for (;;) {
    uint32 flushed_count = 0;
    debug_write("M");
    thread_sleep(seconds_to_time(60).n);
    
    if(mutex_lock_or_timeout(disk_mod.cache_mut, seconds_to_time(60))) {
      cb = NULL;
      deq = &disk_mod.LRU_deq;
      lru_probe = deq->prev;

      while (lru_probe != deq) {
        cb = CAST(cache_block*, CAST(uint8*, lru_probe) - (CAST(uint8*, &cb->LRU_deq) - CAST(uint8*, cb)));

        if (flush_block(cb, seconds_to_time(10)) > 0) {
          flushed_count += 1;
        }

        lru_probe = lru_probe->prev;
      }

      // Done the cleaning task
      mutex_unlock(disk_mod.cache_mut);
    }
  }
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //

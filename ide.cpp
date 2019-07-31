// file: "ide.cpp"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 22 Sep 01  initial version (Marc Feeley)

//-----------------------------------------------------------------------------

#include "ide.h"
#include "asm.h"
#include "disk.h"
#include "intr.h"
#include "rtlib.h"
#include "term.h"
#include "thread.h"

//-----------------------------------------------------------------------------

// IDE controller ports and IRQs.

static const struct {
  uint16 base;
  uint16 irq;
} ide_controller_map[] = {{0x1f0, 14}, {0x170, 15}, {0x1e8, 12}, {0x168, 10}};

//-----------------------------------------------------------------------------

typedef struct ide_module_struct {
  ide_controller ide[IDE_CONTROLLERS];
} ide_module;

static ide_module ide_mod;

ide_cmd_queue_entry* ide_cmd_queue_alloc(ide_device* dev) {
  int32 i;
  ide_controller* ctrl;
  ide_cmd_queue_entry* entry;

  ASSERT_INTERRUPTS_DISABLED();  // Interrupts should be disabled at this point

  ctrl = dev->ctrl;

  while ((i = ctrl->cmd_queue_freelist) < 0)
    ctrl->cmd_queue_condvar->mutexless_wait();

  entry = &ctrl->cmd_queue[i];

  entry->dev = dev;
  entry->refcount = 2;  // the interrupt handler and the client have to free me

  i = entry->next;

  ctrl->cmd_queue_freelist = i;

  if (i >= 0) ctrl->cmd_queue_condvar->mutexless_signal();

  return entry;
}

void ide_cmd_queue_free(ide_cmd_queue_entry* entry) {
  ide_device* dev;
  ide_controller* ctrl;

  ASSERT_INTERRUPTS_DISABLED();  // Interrupts should be disabled at this point

  dev = entry->dev;
  ctrl = dev->ctrl;

  if (--entry->refcount == 0) {
    entry->next = ctrl->cmd_queue_freelist;
    ctrl->cmd_queue_freelist = entry->id;
    ctrl->cmd_queue_condvar->mutexless_signal();
  }
}

void ide_irq(ide_controller* ctrl) {
  uint8 s;
  uint32 i;
  ide_cmd_queue_entry* entry;
  ide_device* dev;
  uint16 base;
  uint16* p;

  entry = &ctrl->cmd_queue[0];  // We only handle one operation at a time
  dev = entry->dev;
  base = ide_controller_map[ctrl->id].base;

  cmd_type type = entry->cmd;

  if (type == cmd_read_sectors) {
    p = CAST(uint16*, entry->_.read_sectors.buf);
  } else if (type == cmd_write_sectors) {
    p = CAST(uint16*, entry->_.write_sectors.buf);
  } else {
    fatal_error("[IDE.CPP] Unknown command type...");
  }

  s = inb(base + IDE_STATUS_REG);

  if (s & IDE_STATUS_ERR) {
#ifdef SHOW_DISK_INFO
    uint8 err = inb(base + IDE_ERROR_REG);

    term_write(cout, "***IDE ERROR***\n");

    if (err & IDE_ERROR_BBK)
      term_write(cout, "Bad block mark detected in sector's ID field\n");
    if (err & IDE_ERROR_UNC)
      term_write(cout, "Uncorrectable data error encountered\n");
    if (err & IDE_ERROR_IDNF)
      term_write(cout, "Requested sector's ID field not found\n");
    if (err & IDE_ERROR_ABRT)
      term_write(cout, "Command aborted (status error or invalid cmd)\n");
    if (err & IDE_ERROR_TK0NF)
      term_write(cout, "Track 0 not found during recalibrate command\n");
    if (err & IDE_ERROR_AMNF)
      term_write(cout, "Data address mark not found after ID field\n");
#endif

    if (type == cmd_read_sectors) {
      entry->_.read_sectors.err = UNKNOWN_ERROR;
    } else if (type == cmd_write_sectors) {
      entry->_.write_sectors.err = UNKNOWN_ERROR;
      fatal_error("[IDE.CPP] Unknown command type...");
    }
  } else if(type == cmd_read_sectors) {
    for (i = entry->_.read_sectors.count << (IDE_LOG2_SECTOR_SIZE - 1); i > 0;
         i--)
      *p++ = inw(base + IDE_DATA_REG);

    if (inb(base + IDE_ALT_STATUS_REG) & IDE_STATUS_DRQ)
      entry->_.read_sectors.err = UNKNOWN_ERROR;
    else
      entry->_.read_sectors.err = NO_ERROR;
  } else if (type == cmd_write_sectors) {
    // Nothing, it's only a flush command
  }

  entry->done->mutexless_signal();
  ide_cmd_queue_free(entry);
}

#ifdef USE_IRQ14_FOR_IDE0

extern "C" void irq14() {
#ifdef SHOW_INTERRUPTS
  term_write(cout, "\033[41m irq14 \033[0m";
#endif

  ACKNOWLEDGE_IRQ(14);

  ide_irq (&ide_mod.ide[0]);
}

#endif

#ifdef USE_IRQ15_FOR_IDE1

extern "C" void irq15() {
#ifdef SHOW_INTERRUPTS
  term_write(cout, "\033[41m irq15 \033[0m";
#endif

  ACKNOWLEDGE_IRQ(15);

  ide_irq (&ide_mod.ide[1]);
}

#endif

error_code __attribute__((optimize("O0"))) ide_read_sectors(ide_device* dev, uint32 lba, void* buf,
                            uint32 count) {
  error_code err = NO_ERROR;

  ASSERT_INTERRUPTS_ENABLED();  // Interrupts should be enabled at this point

  if (count > 0) {
    ide_controller* ctrl = dev->ctrl;
    uint16 base = ide_controller_map[ctrl->id].base;
    ide_cmd_queue_entry* entry;

    disable_interrupts();

    entry = ide_cmd_queue_alloc(dev);

    if (count > 256) count = 256;

    entry->cmd = cmd_read_sectors;
    entry->_.read_sectors.buf = buf;
    entry->_.read_sectors.count = count;

    outb(IDE_DEV_HEAD_LBA | IDE_DEV_HEAD_DEV(dev->id) | (lba >> 24),
         base + IDE_DEV_HEAD_REG);
    outb(count, base + IDE_SECT_COUNT_REG);
    outb(lba, base + IDE_SECT_NUM_REG);
    outb((lba >> 8), base + IDE_CYL_LO_REG);
    outb((lba >> 16), base + IDE_CYL_HI_REG);
    outb(IDE_READ_SECTORS_CMD, base + IDE_COMMAND_REG);

    entry->done->mutexless_wait();

    err = entry->_.read_sectors.err;

    ide_cmd_queue_free(entry);

    enable_interrupts();
  }

  return err;
}

error_code __attribute__((optimize("O0"))) ide_write_sectors(ide_device* dev, uint32 lba, void* buf,
                             uint32 count) {
  error_code err = NO_ERROR;

  ASSERT_INTERRUPTS_ENABLED();  // Interrupts should be enabled at this point

  if (count > 0) {
    ide_controller* ctrl = dev->ctrl;
    uint16 base = ide_controller_map[ctrl->id].base;
    ide_cmd_queue_entry* entry;

    disable_interrupts();


    if (count > 256) count = 256;

    entry = ide_cmd_queue_alloc(dev);
    entry->cmd = cmd_write_sectors;
    entry->_.write_sectors.buf = buf;
    entry->_.write_sectors.count = count;

    outb(IDE_DEV_HEAD_LBA | IDE_DEV_HEAD_DEV(dev->id) | (lba >> 24),
         base + IDE_DEV_HEAD_REG);
    outb(count, base + IDE_SECT_COUNT_REG);
    outb(lba, base + IDE_SECT_NUM_REG);
    outb((lba >> 8), base + IDE_CYL_LO_REG);
    outb((lba >> 16), base + IDE_CYL_HI_REG);
    outb(IDE_WRITE_SECTORS_CMD, base + IDE_COMMAND_REG);

    uint16* p = CAST(uint16*, entry->_.write_sectors.buf);

    for (int i = entry->_.write_sectors.count << (IDE_LOG2_SECTOR_SIZE - 1); i > 0; i--) {
      outw(*p++, base + IDE_DATA_REG);
    }

    if (inb(base + IDE_ALT_STATUS_REG) & IDE_STATUS_DRQ) {
      entry->_.write_sectors.err = UNKNOWN_ERROR;
    } else {
      entry->_.write_sectors.err = NO_ERROR;
    }

    outb(IDE_FLUSH_CACHE_CMD, base + IDE_COMMAND_REG);
    entry->done->mutexless_wait();

    err = entry->_.write_sectors.err;

    ide_cmd_queue_free(entry);

    enable_interrupts();
  }

  return err;
}

error_code __attribute__((optimize("O0"))) ide_write(ide_device* dev, uint32 lba, uint32 wrt_offset, uint32 count, void* buff) {
  error_code err = UNIMPL_ERROR;
//   uint32 sector_count;

//   if(count < 1) {
//     return err;
//   }

//   lba += wrt_offset / (1 << IDE_LOG2_SECTOR_SIZE);
//   wrt_offset = wrt_offset % (1 << IDE_LOG2_SECTOR_SIZE);

//   uint32 sector_sz = (1 << IDE_LOG2_SECTOR_SIZE);

//   if (count == sector_sz) {
//     sector_count = 1;
//   } else {
//     sector_count = count / (1 << IDE_LOG2_SECTOR_SIZE) + 1;
//   }

//   // TODO loop for sector count > 1?

//   if(sector_count > 1) {
//     debug_write(count);
//     fatal_error("Writing more than a single sector is not implemented");
//     return UNIMPL_ERROR;
//   }

//   uint8* sect_buff = (uint8*)kmalloc(
//       sizeof(uint8) * (1 << IDE_LOG2_SECTOR_SIZE) * sector_count);

//   if(NULL == sect_buff) {
//     err = MEM_ERROR;
//     return err;
//   }

//   // Read the sectors currently on the disk
//   if(ERROR(err = ide_read_sectors(dev, lba, sect_buff, sector_count))) {
//     goto ide_write_end;
//   }

//   // Replace the content
//   memcpy(sect_buff + wrt_offset, buff, count);
//   // Rewrite the sectors
//   if(ERROR(err = disk_write_sectors(fs->d, lba, sect_buff, sector_count))) {
//     goto ide_write_end;
//   }

// ide_write_end:
//   if (NULL != sect_buff) {
//     kfree(sect_buff);
//   }
  
  return err;
}

static void swap_and_trim (native_string dst, uint16* src, uint32 len)
{
  uint32 i;
  uint32 end = 0;

  for (i = 0; i < len; i++) {
    dst[i] = CAST(native_char, (i & 1) ? src[i >> 1] : (src[i >> 1] >> 8));
    if (dst[i] != ' ') end = i + 1;
  }

  dst[end] = '\0';
}

static void setup_ide_device(ide_controller* ctrl, ide_device* dev, uint8 id) {
  uint32 i;
  uint32 j;
  uint16 ident[1 << (IDE_LOG2_SECTOR_SIZE - 1)];
  uint16 base;

  dev->id = id;
  dev->ctrl = ctrl;

  if (dev->kind == IDE_DEVICE_ABSENT) return;

  base = ide_controller_map[ctrl->id].base;

  // perform an IDENTIFY DEVICE or IDENTIFY PACKET DEVICE command

  outb(IDE_DEV_HEAD_IBM | IDE_DEV_HEAD_DEV(dev->id), base + IDE_DEV_HEAD_REG);

  if (dev->kind == IDE_DEVICE_ATA) {
    outb(IDE_IDENTIFY_DEVICE_CMD, base + IDE_COMMAND_REG);
  } else {
    outb(IDE_IDENTIFY_PACKET_DEVICE_CMD, base + IDE_COMMAND_REG);
  }

  for (j = 1000000; j > 0; j--)  // wait up to 1 second for a response
  {
    uint8 stat = inb(base + IDE_STATUS_REG);

    if (!(stat & IDE_STATUS_BSY)) {
      if (stat & IDE_STATUS_ERR) j = 0;
      break;
    }

    thread::sleep(1000);  // 1 usec
  }

  if (j == 0) {
    dev->kind = IDE_DEVICE_ABSENT;
    return;
  }

  for (i = 0; i < (1 << (IDE_LOG2_SECTOR_SIZE - 1)); i++)
    ident[i] = inw(base + IDE_DATA_REG);

  swap_and_trim(dev->serial_num, ident + 10, 20);
  swap_and_trim(dev->firmware_rev, ident + 23, 8);
  swap_and_trim(dev->model_num, ident + 27, 40);

  dev->cylinders_per_disk = 0;
  dev->heads_per_cylinder = 0;
  dev->sectors_per_track = 0;
  dev->total_sectors_when_using_CHS = 0;
  dev->total_sectors = 0;

  if (dev->kind == IDE_DEVICE_ATA) {
    dev->cylinders_per_disk = ident[1];
    dev->heads_per_cylinder = ident[3];
    dev->sectors_per_track = ident[6];
    dev->total_sectors = (CAST(uint32, ident[61]) << 16) + ident[60];
  }

  if (dev->kind == IDE_DEVICE_ATA) {
    if (ident[53] & (1 << 0)) {
      dev->cylinders_per_disk = ident[54];
      dev->heads_per_cylinder = ident[55];
      dev->sectors_per_track = ident[56];
      dev->total_sectors_when_using_CHS =
          (CAST(uint32, ident[58]) << 16) + ident[57];
    }
  }

  term_write(cout, "  word 47 hi (should be 128) = ");
  term_write(cout, (ident[47] >> 8));
  term_writeline(cout);
  term_write(cout,
             "  Maximum number of sectors that shall be transferred per "
             "interrupt on READ/WRITE MULTIPLE commands = ");
  term_write(cout, (ident[47] & 0xff));
  term_writeline(cout);

#ifdef SHOW_IDE_INFO

  if (dev->kind == IDE_DEVICE_ATA) {
    if ((ident[0] & (1 << 15)) == 0) term_write(cout, "  ATA device\n");
  } else {
    if ((ident[0] & (3 << 14)) == (2 << 14))
      term_write(cout, "  ATAPI device\n");
  }

  if ((ident[0] & (1 << 7)) == 1)
    term_write(cout, "  removable media device\n");

#if 0

  if ((ident[0] & (1<<6)) == 1)
    term_write(cout, "  not removable controller and/or device\n";

#endif

  if ((ident[0] & (1 << 2)) == 1) term_write(cout, "  response incomplete\n");

  if (dev->kind == IDE_DEVICE_ATA) {
    term_write(cout, "  Number of logical cylinders = ");
    term_write(cout, ident[1]);
    term_write(cout, "\n");
    term_write(cout, "  Number of logical heads = ");
    term_write(cout, ident[3]);
    term_writeline(cout);
    term_write(cout, "  Number of logical sectors per logical track = ");
    term_write(cout, ident[6]);
    term_writeline(cout);
  }

  term_write(cout, "  Serial number = ");
  term_write(cout, dev->serial_num);
  term_writeline(cout);
  term_write(cout, "  Firmware revision = ");
  term_write(cout, dev->firmware_rev);
  term_writeline(cout);
  term_write(cout, "  Model number = ");
  term_write(cout, dev->model_num);
  term_writeline(cout);
#if 0

  term_write(cout, "  word 47 hi (should be 128) = " << (ident[47] >> 8) << "\n";
  term_write(cout, "  Maximum number of sectors that shall be transferred per interrupt on READ/WRITE MULTIPLE commands = " << (ident[47] & 0xff) << "\n";

#endif

  if (dev->kind == IDE_DEVICE_ATA) {
    if (ident[53] & (1 << 0)) {
      term_write(cout, "  Number of current logical cylinders = ");
      term_write(cout, ident[54]);
      term_writeline(cout);
      term_write(cout, "  Number of current logical heads = ");
      term_write(cout, ident[55]);
      term_writeline(cout);
      term_write(cout, "  Number of current logical sectors per track = ");
      term_write(cout, ident[56]);
      term_writeline(cout);
      term_write(cout, "  Current capacity in sectors = ");
      term_write(cout, (CAST(uint32, ident[58]) << 16) + ident[57]);
      term_writeline(cout);
    }

    term_write(cout,
               "  Total number of user addressable sectors (LBA mode only) = ");
    term_write(cout, (CAST(uint32, ident[61]) << 16) + ident[60]);
    term_writeline(cout);
  }

  if (ident[63] & (1 << 10))
    term_write(cout, "  Multiword DMA mode 2 is selected\n");

  if (ident[63] & (1 << 9))
    term_write(cout, "  Multiword DMA mode 1 is selected\n");

  if (ident[63] & (1 << 8))
    term_write(cout, "  Multiword DMA mode 0 is selected\n");

  if (ident[63] & (1 << 2))
    term_write(cout, "  Multiword DMA mode 2 and below are supported\n");

  if (ident[63] & (1 << 1))
    term_write(cout, "  Multiword DMA mode 1 and below are supported\n");

  if (ident[63] & (1 << 0))
    term_write(cout, "  Multiword DMA mode 0 is supported\n");

  term_write(cout, "  Maximum queue depth � 1 = ");
  term_write(cout, (ident[75] & 31));

  if (ident[80] & (1 << 5)) term_write(cout, "  supports ATA/ATAPI-5\n");

  if (ident[80] & (1 << 4)) term_write(cout, "  supports ATA/ATAPI-4\n");

  if (ident[80] & (1 << 3)) term_write(cout, "  supports ATA-3\n");

  if (ident[80] & (1 << 2)) term_write(cout, "  supports ATA-2\n");

  term_write(cout, "  Minor version number = ");
  term_write(cout, ident[81]);

  term_write(cout, "  supports:");

  if (ident[82] & (1 << 14)) term_write(cout, " NOP command,");

  if (ident[82] & (1 << 13)) term_write(cout, " READ BUFFER command,");

  if (ident[82] & (1 << 12)) term_write(cout, " WRITE BUFFER command,");

  if (ident[82] & (1 << 10))
    term_write(cout, " Host Protected Area feature set,");

  if (ident[82] & (1 << 9)) term_write(cout, " DEVICE RESET command,");

  if (ident[82] & (1 << 8)) term_write(cout, " SERVICE interrupt,");

  if (ident[82] & (1 << 7)) term_write(cout, " release interrupt,");

  if (ident[82] & (1 << 6)) term_write(cout, " look-ahead,");

  if (ident[82] & (1 << 5)) term_write(cout, " write cache,");

  if (ident[82] & (1 << 3)) term_write(cout, " Power Management feature set,");

  if (ident[82] & (1 << 2)) term_write(cout, " Removable Media feature set,");

  if (ident[82] & (1 << 1)) term_write(cout, " Security Mode feature set,");

  if (ident[82] & (1 << 0)) term_write(cout, " SMART feature set,");

  if (ident[83] & (1 << 8)) term_write(cout, " SET MAX security extension,");

  if (ident[83] & (1 << 6))
    term_write(cout,
               " SET FEATURES subcommand required to spinup after power-up,");

  if (ident[83] & (1 << 5))
    term_write(cout, " Power-Up In Standby feature set,");

  if (ident[83] & (1 << 4))
    term_write(cout, " Removable Media Status Notification feature set,");

  if (ident[83] & (1 << 3))
    term_write(cout, " Advanced Power Management feature set,");

  if (ident[83] & (1 << 2)) term_write(cout, " CFA feature set,");

  if (ident[83] & (1 << 1)) term_write(cout, " READ/WRITE DMA QUEUED,");

  if (ident[83] & (1 << 0)) term_write(cout, " DOWNLOAD MICROCODE command,");

  term_write(cout, "\n");

  term_write(cout, "  enabled:");

  if (ident[85] & (1 << 14)) term_write(cout, " NOP command,");

  if (ident[85] & (1 << 13)) term_write(cout, " READ BUFFER command,");

  if (ident[85] & (1 << 12)) term_write(cout, " WRITE BUFFER command,");

  if (ident[85] & (1 << 10))
    term_write(cout, " Host Protected Area feature set,");

  if (ident[85] & (1 << 9)) term_write(cout, " DEVICE RESET command,");

  if (ident[85] & (1 << 8)) term_write(cout, " SERVICE interrupt,");

  if (ident[85] & (1 << 7)) term_write(cout, " release interrupt,");

  if (ident[85] & (1 << 6)) term_write(cout, " look-ahead,");

  if (ident[85] & (1 << 5)) term_write(cout, " write cache,");

  if (ident[85] & (1 << 3)) term_write(cout, " Power Management feature set,");

  if (ident[85] & (1 << 2)) term_write(cout, " Removable Media feature set,");

  if (ident[85] & (1 << 1)) term_write(cout, " Security Mode feature set,");

  if (ident[85] & (1 << 0)) term_write(cout, " SMART feature set,");

  if (ident[86] & (1 << 8)) term_write(cout, " SET MAX security extension,");

  if (ident[86] & (1 << 6))
    term_write(cout,
               " SET FEATURES subcommand required to spin-up after power-up,");

  if (ident[86] & (1 << 5))
    term_write(cout, " Power-Up In Standby feature set,");

  if (ident[86] & (1 << 4))
    term_write(cout, " Removable Media Status Notification feature set,");

  if (ident[86] & (1 << 3))
    term_write(cout, " Advanced Power Management feature set,");

  if (ident[86] & (1 << 2)) term_write(cout, " CFA feature set,");

  if (ident[86] & (1 << 1)) term_write(cout, " READ/WRITE DMA QUEUED command,");

  if (ident[86] & (1 << 0)) term_write(cout, " DOWNLOAD MICROCODE command,");

  term_write(cout, "\n");

  if (ident[53] & (1 << 2)) {
    if (ident[88] & (1 << 12))
      term_write(cout, "  Ultra DMA mode 4 is selected\n");

    if (ident[88] & (1 << 11))
      term_write(cout, "  Ultra DMA mode 3 is selected\n");

    if (ident[88] & (1 << 10))
      term_write(cout, "  Ultra DMA mode 2 is selected\n");

    if (ident[88] & (1 << 9))
      term_write(cout, "  Ultra DMA mode 1 is selected\n");

    if (ident[88] & (1 << 8))
      term_write(cout, "  Ultra DMA mode 0 is selected\n");

    if (ident[88] & (1 << 4))
      term_write(cout, "  Ultra DMA mode 4 and below are supported\n");

    if (ident[88] & (1 << 3))
      term_write(cout, "  Ultra DMA mode 3 and below are supported\n");

    if (ident[88] & (1 << 2))
      term_write(cout, "  Ultra DMA mode 2 and below are supported\n");

    if (ident[88] & (1 << 1))
      term_write(cout, "  Ultra DMA mode 1 and below are supported\n");

    if (ident[88] & (1 << 0))
      term_write(cout, "  Ultra DMA mode 0 is supported\n");
  }

#endif
}

static void setup_ide_controller(ide_controller* ctrl, uint8 id) {
  term_write(cout, "Setting up IDE controller no: ");
  term_write(cout, id);
  term_writeline(cout);

  uint32 i;
  uint32 j;
  uint8 stat[IDE_DEVICES_PER_CONTROLLER];
  uint8 err;
  uint8 candidates;
  uint16 base = ide_controller_map[id].base;

  ctrl->id = id;

  // check each device to see if it is present

  candidates = 0;

  for (i = 0; i < IDE_DEVICES_PER_CONTROLLER; i++) {
#ifdef SHOW_IDE_INFO
    term_write(cout, "Device of controller no: ");
    term_write(cout, i);
    term_writeline(cout);
#endif

    outb(IDE_DEV_HEAD_IBM | IDE_DEV_HEAD_DEV(i), base + IDE_DEV_HEAD_REG);
#ifdef SHOW_IDE_INFO
    term_write(cout, "[START] Sleeping 400 nsecs");
#endif

    thread::sleep(400);  // 400 nsecs
#ifdef SHOW_IDE_INFO
    term_write(cout, "[STOP ] Sleeping 400 nsecs");
#endif

    stat[i] = inb(base + IDE_STATUS_REG);

    if ((stat[i] & (IDE_STATUS_BSY | IDE_STATUS_DRDY | IDE_STATUS_DF |
                    IDE_STATUS_DSC | IDE_STATUS_DRQ)) !=
        (IDE_STATUS_BSY | IDE_STATUS_DRDY | IDE_STATUS_DF | IDE_STATUS_DSC |
         IDE_STATUS_DRQ)) {
      ctrl->device[i].kind = IDE_DEVICE_ATAPI;  // for now means the device
      candidates++;                             // is possibly present
    } else {
      ctrl->device[i].kind = IDE_DEVICE_ABSENT;
    }
  }

  if (candidates > 0) {
    // perform a software RESET of the IDE controller
    term_write(cout, "Resetting the IDE...");

    outb(IDE_DEV_HEAD_IBM | IDE_DEV_HEAD_DEV(0), base + IDE_DEV_HEAD_REG);
    thread::sleep(400);  // 400 nsecs
    inb(base + IDE_STATUS_REG);

    thread::sleep(5000);  // 5 usecs
    outb(IDE_DEV_CTRL_nIEN, base + IDE_DEV_CTRL_REG);
    thread::sleep(5000);  // 5 usecs
    outb(IDE_DEV_CTRL_nIEN | IDE_DEV_CTRL_SRST, base + IDE_DEV_CTRL_REG);
    thread::sleep(5000);  // 5 usecs
    outb(IDE_DEV_CTRL_nIEN, base + IDE_DEV_CTRL_REG);
    thread::sleep(2000000);  // 2 msecs
    err = inb(base + IDE_ERROR_REG);
    thread::sleep(5000);  // 5 usecs

    for (j = 30000; j > 0; j--)  // wait up to 30 seconds for a response
    {
      for (i = 0; i < IDE_DEVICES_PER_CONTROLLER; i++) {
        term_write(cout, "Reset of device ");
        term_write(cout, i);
        term_writeline(cout);
        outb(IDE_DEV_HEAD_IBM | IDE_DEV_HEAD_DEV(i), base + IDE_DEV_HEAD_REG);
        thread::sleep(400);  // 400 nsecs
        if (((inb(base + IDE_STATUS_REG) & IDE_STATUS_BSY) == 0) &&
            ctrl->device[i].kind == IDE_DEVICE_ATAPI) {
          ctrl->device[i].kind = IDE_DEVICE_ATA;
          candidates--;
        }
      }

      if (candidates == 0) break;

      thread::sleep(1000000);  // 1 msec
    }

    candidates = 0;

    for (i = 0; i < IDE_DEVICES_PER_CONTROLLER; i++) {
      if (ctrl->device[i].kind == IDE_DEVICE_ATA)
        candidates++;
      else
        ctrl->device[i].kind = IDE_DEVICE_ABSENT;
    }

    if (candidates > 0) {
      for (i = 0; i < IDE_DEVICES_PER_CONTROLLER; i++) {
        outb(IDE_DEV_HEAD_IBM | IDE_DEV_HEAD_DEV(i), base + IDE_DEV_HEAD_REG);
        thread::sleep(400);  // 400 nsecs

        if (inb(base + IDE_CYL_LO_REG) == 0x14 &&
            inb(base + IDE_CYL_HI_REG) == 0xeb &&
            ctrl->device[i].kind == IDE_DEVICE_ATA)
          ctrl->device[i].kind = IDE_DEVICE_ATAPI;
      }

      for (i = 0; i < IDE_DEVICES_PER_CONTROLLER; i++) {
        if (ctrl->device[i].kind == IDE_DEVICE_ATA) {
          if (stat[i] == 0)
            ctrl->device[i].kind = IDE_DEVICE_ABSENT;
          else {
            outb(IDE_DEV_HEAD_IBM | IDE_DEV_HEAD_DEV(i),
                 base + IDE_DEV_HEAD_REG);
            thread::sleep(400);  // 400 nsecs

            outb(0x58, base + IDE_ERROR_REG);
            outb(0xa5, base + IDE_CYL_LO_REG);
            if (inb(base + IDE_ERROR_REG) == 0x58 ||
                inb(base + IDE_CYL_LO_REG) != 0xa5)
              ctrl->device[i].kind = IDE_DEVICE_ABSENT;
          }
        }
      }
    }
  }

  // setup each device

  candidates = 0;

  for (i = 0; i < IDE_DEVICES_PER_CONTROLLER; i++) {
#ifdef SHOW_IDE_INFO

    if (ctrl->device[i].kind != IDE_DEVICE_ABSENT) {
      term_write(cout, "IDE");
      term_write(cout, ctrl->id);
      term_write(cout, ".");
      term_write(cout, "i");

      if (ctrl->device[i].kind == IDE_DEVICE_ATA) {
        term_write(cout, " is ATA\n");
      } else {
        term_write(cout, " is ATAPI\n");
      }
    }

#endif

    setup_ide_device(ctrl, &ctrl->device[i], i);

    if (ctrl->device[i].kind != IDE_DEVICE_ABSENT) candidates++;
  }

  // setup command queue

  for (i = 0; i < MAX_NB_IDE_CMD_QUEUE_ENTRIES; i++) {
    ide_cmd_queue_entry* entry = &ctrl->cmd_queue[i];
    entry->id = i;
    entry->next = i + 1;
    entry->done = new condvar;
  }

  ctrl->cmd_queue[MAX_NB_IDE_CMD_QUEUE_ENTRIES - 1].next = -1;

  ctrl->cmd_queue_freelist = 0;
  ctrl->cmd_queue_condvar = new condvar;

  if (candidates > 0) {
    // enable interrupts

    outb(0, base + IDE_DEV_CTRL_REG);
    thread::sleep(2000000);  // 2 msecs

    ENABLE_IRQ(ide_controller_map[id].irq);
  }
}

void setup_ide() {
  uint32 i;
  uint32 j;

  for (i = 0; i < IDE_CONTROLLERS; i++)
    setup_ide_controller(&ide_mod.ide[i], i);

  for (i = 0; i < IDE_CONTROLLERS; i++)
    for (j = 0; j < IDE_DEVICES_PER_CONTROLLER; j++)
      if (ide_mod.ide[i].device[j].kind != IDE_DEVICE_ABSENT) {
        disk* d = disk_alloc();
        if (d != NULL) {
          d->kind = DISK_IDE;
          d->log2_sector_size = IDE_LOG2_SECTOR_SIZE;
          d->partition_type = 0;
          d->partition_path = 0;
          d->partition_start = 0;
          d->partition_length = ide_mod.ide[i].device[j].total_sectors;
          d->_.ide.dev = &ide_mod.ide[i].device[j];
        }
      }
}

//-----------------------------------------------------------------------------

// Local Variables: //
// mode: C++ //
// End: //

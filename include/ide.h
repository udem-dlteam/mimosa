// file: "ide.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 01 Nov 01  initial version (Marc Feeley)

#ifndef __IDE_H
#define __IDE_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "thread.h"

//-----------------------------------------------------------------------------

//
// Definitions for Intelligent Drive Electronics (IDE) disks.
//

#define IDE_DEVICE_ABSENT 0
#define IDE_DEVICE_ATA 1
#define IDE_DEVICE_ATAPI 2

#define IDE_CONTROLLERS 4
#define IDE_DEVICES_PER_CONTROLLER 2

#define IDE_DATA_REG 0       // 16 bit, data I/O
#define IDE_ERROR_REG 1      // 8 bit, error
#define IDE_FEATURES_REG 1   // 8 bit, features
#define IDE_SECT_COUNT_REG 2 // 8 bit, sector count
#define IDE_SECT_NUM_REG 3   // 8 bit, sector number
#define IDE_CYL_LO_REG 4     // 8 bit, LSB of cylinder
#define IDE_CYL_HI_REG 5     // 2 bit, MSB of cylinder
#define IDE_DEV_HEAD_REG 6   // 8 bit, 1 LBA 1 DRV HD3 HD2 HD1 HD0
#define IDE_STATUS_REG 7
#define IDE_ALT_STATUS_REG 0x0C
#define IDE_COMMAND_REG 7
#define IDE_DEV_CTRL_REG 0x0C
#define IDE_DRIVE_ADDR_REG 0xD

#define IDE_STATUS_BSY (1 << 7)   // Device busy bit
#define IDE_STATUS_RDY (1 << 6)   // Device ready bit
#define IDE_STATUS_DF (1 << 5)    // Device fault bit
#define IDE_STATUS_DSC (1 << 4)   // Drive seek complete bit
#define IDE_STATUS_DRQ (1 << 3)   // Data request bit
#define IDE_STATUS_CORR (1 << 2)  // Corrected data bit
#define IDE_STATUS_INDEX (1 << 1) // Index bit
#define IDE_STATUS_ERR (1 << 0)   // Error bit

#define IDE_ERROR_BBK (1 << 7)  // Bad block mark detected in sector's ID field
#define IDE_ERROR_UNC (1 << 6)  // Uncorrectable data error encountered
#define IDE_ERROR_IDNF (1 << 4) // Requested sector's ID field not found
#define IDE_ERROR_ABRT (1 << 2) // Command aborted (status error or invalid cmd)
#define IDE_ERROR_TK0NF (1 << 1) // Track 0 not found during recalibrate command
#define IDE_ERROR_AMNF (1 << 0)  // Data address mark not found after ID field

#define IDE_DEV_CTRL_SRST (1 << 2) // Software reset bit
#define IDE_DEV_CTRL_nIEN (1 << 1) // Interrupt enable bit (0=enabled)

#define IDE_DEV_HEAD_IBM 0xa0
#define IDE_DEV_HEAD_LBA ((1 << 6) | IDE_DEV_HEAD_IBM) // LBA address
#define IDE_DEV_HEAD_DEV(x) ((x) << 4)                 // Device index (0 or 1)

#define IDE_EXEC_DEVICE_DIAG_CMD 0x90
#define IDE_FLUSH_CACHE_CMD 0xe7
#define IDE_IDENTIFY_DEVICE_CMD 0xec
#define IDE_IDENTIFY_PACKET_DEVICE_CMD 0xa1
#define IDE_IDLE_CMD 0xe3
#define IDE_IDLE_IMMEDIATE_CMD 0xe1
#define IDE_MEDIA_EJECT_CMD 0xed
#define IDE_MEDIA_LOCK_CMD 0xde
#define IDE_MEDIA_UNLOCK_CMD 0xdf
#define IDE_NOP_CMD 0x00
#define IDE_READ_DMA_CMD 0xc8
#define IDE_READ_DMA_QUEUED_CMD 0xc7
#define IDE_READ_MULTIPLE_CMD 0xc4
#define IDE_READ_SECTORS_CMD 0x20
#define IDE_SEEK_CMD 0x70
#define IDE_SET_FEATURES_CMD 0xef
#define IDE_WRITE_DMA_CMD 0xca
#define IDE_WRITE_DMA_QUEUED_CMD 0xcc
#define IDE_WRITE_MULTIPLE_CMD 0xc5
#define IDE_WRITE_SECTORS_CMD 0x30

#define IDE_BAR_MASK 0xFFFFFFFC
#define IDE_PATA_FIRST_CONTROLLER_BASE 0x1F0
#define IDE_PATA_FIRST_CONTROLLER 0x3F6
#define IDE_PATA_SECOND_CONTROLLER_BASE 0x170
#define IDE_PATA_SECOND_CONTROLLER 0x376
#define IDE_PCI_PATA_PROG_IF_A (0x80)
#define IDE_PCI_PATA_PROG_IF_B (0x8A)

#define IDE_LOG2_SECTOR_SIZE 9
#define MAX_NB_IDE_CMD_QUEUE_ENTRIES 1

#define IDE_PATA_PRIMARY_IRQ 14
#define IDE_PATA_SECONDARY_IRQ 15

typedef enum { cmd_read_sectors, cmd_write_sectors, cmd_flush_cache } cmd_type;

typedef struct ide_cmd_queue_entry_struct {
  uint8 id; // index of entry in cmd_queue
  uint8 refcount;
  struct ide_device_struct *dev; // the device of this command
  int32 next; // index of next free entry (-1 means end of list)
  condvar *done;
  cmd_type cmd;
  union {
    struct {
      void *buf;
      uint32 count;
      error_code err;
    } read_sectors;
    struct {
      void *buf;
      uint8 count;
      uint8 written;
      error_code err;
    } write_sectors;
  } _;
} ide_cmd_queue_entry;

typedef struct ide_controller_struct ide_controller;

typedef struct ide_device_struct {
  uint8 id;   // 0 to IDE_DEVICES_PER_CONTROLLER-1
  uint8 kind; // IDE_DEVICE_ATA, IDE_DEVICE_ATAPI, or IDE_DEVICE_ABSENT
  struct ide_controller_struct *ctrl; // the controller of this device
  native_char serial_num[20 + 1];
  native_char firmware_rev[8 + 1];
  native_char model_num[40 + 1];

  // for ATA devices:

  uint16 cylinders_per_disk;
  uint16 heads_per_cylinder;
  uint16 sectors_per_track;
  uint32 total_sectors_when_using_CHS;
  uint32 total_sectors;
} ide_device;

struct ide_controller_struct {
  uint8 id;
  uint8 enabled;
  uint8 serial;
  uint16 base_port;
  uint16 controller_port;
  uint16 bus_master_port;
  uint16 irq;
  ide_device device[IDE_DEVICES_PER_CONTROLLER];
  ide_cmd_queue_entry cmd_queue[MAX_NB_IDE_CMD_QUEUE_ENTRIES];
  volatile int cmd_queue_freelist;
  condvar *cmd_queue_condvar;
};

extern ide_controller ide_controller_map[IDE_CONTROLLERS];

error_code ide_read_sectors(ide_device *dev, uint32 sector_pos, void *buf,
                            uint32 count);

error_code ide_write_sectors(ide_device *dev, uint32 sector_pos, void *buf,
                             uint32 count);

void setup_ide();

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //

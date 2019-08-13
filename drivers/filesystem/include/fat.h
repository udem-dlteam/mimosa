#ifndef __FAT_H
#define __FAT_H

#include "general.h"
#include "disk.h"
#include "ide.h"
#include "vfs.h"

#define FAT12_FS 0
#define FAT16_FS 1
#define FAT32_FS 2

#define FAT_UNUSED_ENTRY 0xE5

#define FAT_NAME_MAX 1024
#define DT_UNKNOWN 0
#define DT_DIR 1
#define DT_REG 2

#define FAT_ATTR_READ_ONLY 0x01
#define FAT_ATTR_HIDDEN 0x02
#define FAT_ATTR_SYSTEM 0x04
#define FAT_ATTR_VOLUME_ID 0x08
#define FAT_ATTR_DIRECTORY 0x10
#define FAT_ATTR_ARCHIVE 0x20
#define FAT_ATTR_LONG_NAME \
  (FAT_ATTR_READ_ONLY | FAT_ATTR_HIDDEN | FAT_ATTR_SYSTEM | FAT_ATTR_VOLUME_ID)

#define FAT_NAME_LENGTH 11
#define FAT_DIR_ENTRY_SIZE 32


// Layout of the combined Boot Sector (BS) and BIOS Parameter Block
// (BPB).  The BPB describes the format of the file system if it is
// FAT12, FAT16 or FAT32.

typedef struct BIOS_Parameter_Block_struct {
  uint8 BS_jmpBoot[3];
  uint8 BS_OEMName[8];
  uint8 BPB_BytsPerSec[2];  // 512, 1024, 2048 or 4096
  uint8 BPB_SecPerClus;     // 1, 2, 4, 8, 16, 32, 64 or 128
  uint8 BPB_RsvdSecCnt[2];  // 1 for FAT12 and FAT16, typically 32 for FAT32
  uint8 BPB_NumFATs;        // should be 2
  uint8 BPB_RootEntCnt[2];
  uint8 BPB_TotSec16[2];
  uint8 BPB_Media;
  uint8 BPB_FATSz16[2];
  uint8 BPB_SecPerTrk[2];
  uint8 BPB_NumHeads[2];
  uint8 BPB_HiddSec[4];
  uint8 BPB_TotSec32[4];
  union {
    struct {
      uint8 BS_DrvNum;
      uint8 BS_Reserved1;
      uint8 BS_BootSig;
      uint8 BS_VolID[4];
      uint8 BS_VolLab[11];
      uint8 BS_FilSysType[8];
    } FAT1216;
    struct {
      uint8 BPB_FATSz32[4];
      uint8 BPB_ExtFlags[2];
      uint8 BPB_FSVer[2];
      uint8 BPB_RootClus[4];
      uint8 BPB_FSInfo[2];
      uint8 BPB_BkBootSec[2];
      uint8 BPB_Reserved[12];
      uint8 BS_DrvNum;
      uint8 BS_Reserved1;
      uint8 BS_BootSig;
      uint8 BS_VolID[4];
      uint8 BS_VolLab[11];
      uint8 BS_FilSysType[8];
    } FAT32;
  } _;
} BIOS_Parameter_Block;

typedef struct fat_file_system_struct {
  fs_header header;
  uint8 kind;  // FAT12_FS, FAT16_FS or FAT32_FS
  union {
    struct {
      disk* d;
      uint8 log2_bps;  // log2 (bytes per sector)
      uint8 log2_spc;  // log2 (sectors per cylinder)
      uint32 total_sectors;
      uint32 reserved_sectors;
      uint32 root_directory_sectors;
      uint32 first_data_sector;
      uint32 total_data_clusters;
    } FAT121632;
  } _;
} fat_file_system;

typedef struct fat_file {
  file header;
  uint32 first_cluster;
  uint32 current_cluster;          // the "logical cluster"
  uint32 current_section_start;    // the current LBA of the section
  uint32 current_section_length;   // length in bytes of the current section
  uint32 current_section_pos;      // the offset in bytes from the section
  uint32 current_pos;              // the absolute position
  uint32 length;                   // in bytes
  struct {
    uint32 first_cluster;
  } parent;
  struct {
    // This substruct allows to keep the information
    // of the root directory entry. This allows quick
    // modifications of the entry because we know exactly
    // where it is on the disk.
    uint32 position;
  } entry;
} fat_file;

typedef struct FAT_directory_entry_struct {
  uint8 DIR_Name[FAT_NAME_LENGTH];
  uint8 DIR_Attr;
  uint8 DIR_NTRes;
  uint8 DIR_CrtTimeTenth;
  uint8 DIR_CrtTime[2];
  uint8 DIR_CrtDate[2];
  uint8 DIR_LstAccDate[2];
  uint8 DIR_FstClusHI[2];
  uint8 DIR_WrtTime[2];
  uint8 DIR_WrtDate[2];
  uint8 DIR_FstClusLO[2];
  uint8 DIR_FileSize[4];
} FAT_directory_entry;


error_code mount_fat(vfnode* parent);

error_code fat_open_file(native_string path, file_mode mode, file** f);

void inline set_dir_entry_size(FAT_directory_entry* de, uint32 sz);

error_code stat(native_string path, struct stat* buf);


#endif
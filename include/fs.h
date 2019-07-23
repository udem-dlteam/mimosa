// file: "fs.h"

// Copyright (c) 2001 by Marc Feeley and Universit� de Montr�al, All
// Rights Reserved.
//
// Revision History
// 17 Nov 01  initial version (Marc Feeley)

#ifndef __FS_H
#define __FS_H

//-----------------------------------------------------------------------------

#include "general.h"
#include "disk.h"

//-----------------------------------------------------------------------------

//
// Definitions for file system.
//

#define FAT12_FS 0
#define FAT16_FS 1
#define FAT32_FS 2


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

typedef struct file_system_struct
  {
    uint8 kind; // FAT12_FS, FAT16_FS or FAT32_FS
    union
      {
        struct
          {
            disk* d;
            uint8 log2_bps; // log2 (bytes per sector)
            uint8 log2_spc; // log2 (sectors per cylinder)
            uint32 total_sectors;
            uint32 reserved_sectors;
            uint32 root_directory_sectors;
            uint32 first_data_sector;
            uint32 total_data_clusters;
          } FAT121632;
      } _;
  } file_system;

typedef struct file_struct
  {
    file_system* fs;
    uint32 current_cluster;
    uint32 current_section_start;
    uint32 current_section_length;
    uint32 current_section_pos;
    uint32 current_pos;
    uint32 length; // in bytes
    uint8 mode;
  } file;

error_code open_file (native_string path, file** f);
error_code close_file (file* f);
error_code read_file (file* f, void* buf, uint32 count);

#define NAME_MAX 1024

#define DT_UNKNOWN 0
#define DT_DIR     1
#define DT_REG     2

struct dirent
  {
    uint8 d_type;
    native_char d_name[NAME_MAX+1];
  };

typedef struct DIR_struct
  {
    struct dirent ent;
    file* f;
  } DIR;

DIR* opendir (native_string path);
struct dirent* readdir (DIR* dir);
error_code closedir (DIR* dir);

#define	S_IFREG 1
#define	S_IFDIR 2

#define S_ISREG(m) ((m) == S_IFREG)
#define S_ISDIR(m) ((m) == S_IFDIR)

struct stat
  {
    uint8 st_mode;
    uint32 st_size;
  };

error_code stat (native_string path, struct stat* buf);

void setup_fs ();

//-----------------------------------------------------------------------------

#endif

// Local Variables: //
// mode: C++ //
// End: //

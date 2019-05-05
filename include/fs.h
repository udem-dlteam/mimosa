// file: "fs.h"

// Copyright (c) 2001 by Marc Feeley and Université de Montréal, All
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

#ifndef __FAT_32_H
#define __FAT_32_H

#include "general.h"
#include "fs.h"

#define FAT_32_EOF 0x0ffffff8

error_code fat_32_open_root_dir(file_system* fs,file* f, file** result);

error_code fat_32_find_first_empty_cluster(file_system* fs, uint32* cluster);

error_code fat_32_get_fat_link_value(file_system* fs, uint32 cluster, uint32* value);

error_code fat_32_set_fat_link_value(file_system* fs, uint32 cluster, uint32 value);

#endif

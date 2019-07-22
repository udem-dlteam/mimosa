#ifndef __FAT_32_H
#define __FAT_32_H

#include "general.h"
#include "fs.h"

int fat_32_get_int();

error_code fat_32_open_file (native_string path, file** f);
error_code fat_32_close_file (file* f);
error_code fat_32_read_file (file* f, void* buf, uint32 count);

DIR* fat_32_opendir (native_string path);
struct dirent* fat_32_readdir (DIR* dir);
error_code fat_32_closedir (DIR* dir);

#endif

#ifndef __FAT_32_H
#define __FAT_32_H

#include "general.h"
#include "fs.h"

error_code fat_32_open_root_dir(file_system* fs,file* f, file** result);

#endif

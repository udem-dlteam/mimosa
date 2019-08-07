#include "general.h"
#include "rtlib.h"
#include "ide.h"
#include "disk.h"
#include "fat32.h"
#include "include/vfs.h"
#include "include/fat.h"

error_code init_fat() {
    debug_write("Init FAT");
    return NO_ERROR;
}
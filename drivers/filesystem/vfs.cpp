#include "include/vfs.h"
#include "include/fat.h"
#include "general.h"
#include "term.h"

error_code init_vfs() {
    debug_write("Init VFS");
    init_fat();


    return 0;
}

error_code file_open(native_string path, native_string mode, file** result) {
    return 0;
}

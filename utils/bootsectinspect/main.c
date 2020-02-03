#pragma clang diagnostic push
#pragma ide diagnostic ignored "OCUnusedGlobalDeclarationInspection"
#pragma ide diagnostic ignored "hicpp-signed-bitwise"
#pragma ide diagnostic ignored "readability-non-const-parameter"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FAT_NAME_LENGTH 11
#define FAT_EOC_TAG 0x0FFFFFF8
#define FAT_DIR_ENTRY_SIZE 32
#define HAS_NO_ERROR(err) ((err) >= 0)
#define NO_ERR 0
#define GENERAL_ERR -1
#define OUT_OF_MEM -3
#define RES_NOT_FOUND -4
#define CAST(t, e) ((t) (e))
#define as_uint16(x) \
((CAST(uint16,(x)[1])<<8U)+(x)[0])
#define as_uint32(x) \
((((((CAST(uint32,(x)[3])<<8U)+(x)[2])<<8U)+(x)[1])<<8U)+(x)[0])

typedef unsigned char uint8;
typedef uint8 bool;
typedef unsigned short uint16;
typedef unsigned int uint32;
typedef int error_code;

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
} BPB;

error_code read_boot_block(FILE *archive, BPB **block) {
    BPB *b = CAST(BPB *, malloc(sizeof(BPB)));

    fseek(archive, 0, SEEK_SET);

    if (NULL == b) {
        return OUT_OF_MEM;
    }

    fread(b, sizeof(BPB), 1, archive);
    *block = b;

    return NO_ERR;
}

int main(int argc, char *argv[]) {

    char *output = NULL;
    char *archive_path =
            "../floppy.img";
    FILE *archive = fopen(archive_path, "r");

    char *path = "/home/sam/long.txt";
    FAT_entry *entry;
    BPB *block;
    read_boot_block(archive, &block);
    find_file_descriptor(archive, block, path, &entry);

    if (NULL == entry) {
        printf("Not found...");
        return 0;
    }

    char *content_of_long = malloc(sizeof(uint8) * as_uint32(entry->DIR_FileSize));
    error_code read = read_file(archive, block, entry, content_of_long, 2048 * 2);

    printf("\n%s\n", content_of_long);


    if (NULL != archive) fclose(archive);
    free(entry);
    free(block);
    free(content_of_long);

    return 0;
}



#pragma clang diagnostic push
#pragma ide diagnostic ignored "OCUnusedGlobalDeclarationInspection"
#pragma ide diagnostic ignored "hicpp-signed-bitwise"
#pragma ide diagnostic ignored "readability-non-const-parameter"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HAS_NO_ERROR(err) ((err) >= 0)
#define NO_ERR 0
#define OUT_OF_MEM -3
#define CAST(t, e) ((t) (e))
#define as_uint16(x) \
((CAST(uint16,(x)[1])<<8U)+(x)[0])
#define as_uint32(x) \
((((((CAST(uint32,(x)[3])<<8U)+(x)[2])<<8U)+(x)[1])<<8U)+(x)[0])

typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned int uint32;
typedef uint8 u8;
typedef uint8 bool;
typedef uint16 u16;
typedef uint32 u32;
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

void print_bpb(BPB *bpb) {
#define nline() do {printf("\n");} while(0)
    printf("--- Boot Block ----");
    printf("OEM Name: ");
    for (u8 i = 0; i < 8; ++i) {
        printf("%c", bpb->BS_OEMName[i]);
    }
    nline();
    printf("Bytes per sector: %d", as_uint16(bpb->BPB_BytsPerSec));
    nline();
    printf("Sectors per cluster: %d", bpb->BPB_SecPerClus);
    nline();
    printf("Reserved sector count: %d", as_uint16(bpb->BPB_RsvdSecCnt));
    nline();
    printf("Number of fats: %d", bpb->BPB_NumFATs);
    nline();
    printf("Number of root entries: %d", as_uint16(bpb->BPB_RootEntCnt));
    nline();
    printf("Total sectors (16): %d", as_uint16(bpb->BPB_TotSec16));
    nline();
    printf("Media: %d", bpb->BPB_Media);
    nline();
    printf("FAT size 16: %d", as_uint16(bpb->BPB_FATSz16));
    nline();
    printf("Sectors per track: %d", as_uint16(bpb->BPB_SecPerTrk));
    nline();
    printf("Number of heads: %d", as_uint16(bpb->BPB_NumHeads));
    nline();
    printf("Hidden sectors: %d", as_uint32(bpb->BPB_HiddSec));
    nline();
    printf("--- FAT 32 section ---\n");
    printf("Total sector (32): %d", as_uint32(bpb->BPB_TotSec32));
    nline();
    printf("Fat size (32): %d", as_uint32(bpb->BPB_FATSz32));
    nline();
    printf("Ext flags: %d", as_uint16(bpb->BPB_ExtFlags));
    nline();
    printf("FS version %d", as_uint16(bpb->BPB_FSVer));
    nline();
    printf("Root cluster %d", as_uint32(bpb->BPB_RootClus));
    nline();
    printf("FS Info: %d", as_uint16(bpb->BPB_FSInfo));
    nline();
    printf("Boot sector: %d", as_uint16(bpb->BPB_BkBootSec));
    nline();
    printf("Reserved: ");

    for (u8 i = 0; i < 12; ++i) {
        printf("%c", bpb->BPB_Reserved[i]);
    }

    nline();
    printf("Drive number: %d", bpb->BS_DrvNum);
    nline();
    printf("Reserved 1: %d", bpb->BS_Reserved1);
    nline();
    printf("Boot sig: %d", bpb->BS_BootSig);
    nline();
    printf("Vol ID: %d", as_uint32(bpb->BS_VolID));
    nline();
    printf("Volume label: ");

    for (u8 i = 0; i < 11; ++i) {
        printf("%c", bpb->BS_VolLab[i]);
    }

    nline();
    printf("FS type: ");

    for (u8 i = 0; i < 8; ++i) {
        printf("%c", bpb->BS_FilSysType[i]);
    }
    nline();
#undef nline
}


int main(int argc, char *argv[]) {

    if (1 >= argc) {
        printf("Error: expected a path.\n");
    } else {
        char *path = argv[1];
        printf("Opening %s\n", path);
        FILE *archive = fopen(path, "r");

        if (NULL == archive) {
            printf("Failed to open the archive. Check the path.\n");
        } else {
            BPB *bpb = NULL;
            if (NULL == (bpb = malloc(sizeof(BPB)))) {
                printf("Out of memory...\n");
            } else {
                if (HAS_NO_ERROR(read_boot_block(archive, &bpb))) {
                    print_bpb(bpb);
                } else {
                    printf("Failed to parse the boot block...");
                }
            }

            if (NULL != bpb) {
                free(bpb);
            }
        }

        fclose(archive);
    }


    return 0;
}



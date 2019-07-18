#pragma clang diagnostic push
#pragma ide diagnostic ignored "hicpp-signed-bitwise"

#include <stdio.h>


const int reserved_sectors = 32;
const int hidden_sectors = 0;
const int bytes_per_entry = 32;
const int bytes_per_sector = 512;
const int nb_of_fats = 2;
const int nb_sectors_per_fat = 4088;
const int sectors_per_cluster = 8;

typedef unsigned char byte;
typedef unsigned char uint8;       // 8 bit unsigned integers
typedef unsigned short uint16;
typedef unsigned int uint32;

typedef struct root_dir_entry {
    uint8 DIR_Name[11];
    uint8 DIR_Attr;
    uint8 DIR_NTRes;
    uint8 DIR_CrtTimeTenth;
    uint8 DIR_CrtTime[2];
    uint8 DIR_CrtDate[2];
    uint8 DIR_LstAccDate[2];
    uint16 DIR_FstClusHI;
    uint8 DIR_WrtTime[2];
    uint8 DIR_WrtDate[2];
    uint16 DIR_FstClusLO;
    uint8 DIR_FileSize[4];
} root_dir_entry;


byte SCRATCH[512];

void read_sector(int lba, byte *buff) {
    FILE *fp;
    fp = fopen("/home/syvon/Desktop/fat32/test.img", "r");


    for (int i = 0; i < (bytes_per_sector * lba); ++i) {
        fgetc(fp);
    }

    for (int i = 0; i < bytes_per_sector; ++i) {
        buff[i] = (byte) fgetc(fp);
    }

    fclose(fp);
}

uint32 fetch_next_cluster(uint32 current_cluster) {
    uint32 real_cluster = 0x0FFFFFFFF & current_cluster;
    uint32 sectors_of_fat = real_cluster / (bytes_per_sector / bytes_per_entry);
    uint32 remainder = real_cluster % (bytes_per_sector / bytes_per_entry);

    // Get the offset of the FAT
    int fat_lba_offset = reserved_sectors + hidden_sectors + sectors_of_fat;
    read_sector(fat_lba_offset, SCRATCH);

    uint32 *next = (uint32 *) &SCRATCH[remainder * 4];
    return *next;
}

int cluster_to_lba(uint32 cluster) {
    return (nb_of_fats * nb_sectors_per_fat) + hidden_sectors + reserved_sectors + (cluster - 2) * 8;
}

int main() {
    printf("Hello, World!\n");
    int root_dir_lba = reserved_sectors + hidden_sectors + (nb_of_fats * nb_sectors_per_fat);
    read_sector(root_dir_lba, SCRATCH);

    root_dir_entry *entry_one = (root_dir_entry *) SCRATCH;

    FILE *boot_sys = fopen("/home/syvon/Desktop/fat32/boot.sys", "w");

    uint32 cluster = (entry_one->DIR_FstClusHI << 16) + entry_one->DIR_FstClusLO;

    while (cluster < 0xFFFFFF8) {
        int lba_to_read = cluster_to_lba(cluster);

        for (int lba = lba_to_read; lba < lba_to_read + 8; ++lba) {
            read_sector(lba, SCRATCH);
            for (int i = 0; i < bytes_per_sector; ++i) {
                fputc(SCRATCH[i], boot_sys);
            }
        }

        printf("Cluster: %i \r\n", cluster);
        cluster = fetch_next_cluster(cluster);
    }

    fclose(boot_sys);


    return 0;
}

#pragma clang diagnostic pop
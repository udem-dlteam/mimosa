#!/bin/python3
# Compute the FAT32 checksum

if __name__ == '__main__':
    short_file_name = str(input()).upper()

    if len(short_file_name) != 11:
        raise Exception("Incorrect argument. Expected a short file name")

    s = 0
    for c in short_file_name:
        s = 0xFF & (0x80 if s % 2 == 1 else 0) + (s >> 1) + ord(c)
    print(s)

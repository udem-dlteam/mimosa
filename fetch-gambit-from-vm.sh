#!/bin/bash

scp -oKexAlgorithms=+diffie-hellman-group1-sha1 -P 10022 administrator@localhost:~/libc/gsi.exe archive-items/gambit/bin/gsi.exe
scp -oKexAlgorithms=+diffie-hellman-group1-sha1 -P 10022 administrator@localhost:~/libc/gsc.exe archive-items/gambit/bin/gsc.exe

#!/bin/bash

scp -P 10022 administrator@localhost:~/libc/gsi.exe ./gsi.exe
mv ./gsi.exe archive-items/gambit/bin/gsi.exe

#!/bin/bash
# This script copies modules that have to be compiled to the
# Gambit build VM.

set -e 

scp -P 10022 ~/Documents/gambit/gsc/makefile.compile administrator@localhost:~/makefile.in
scp -P 10022 scheme/keyboard.c administrator@localhost:~/keyboard.c

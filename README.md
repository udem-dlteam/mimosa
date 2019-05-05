Quick-install instructions
==========================

The mimosa repository contains the sources for a minimal operating
system for x86.  It is intended as a basis for benchmarking and
running Gambit.

Assuming the ubuntu-6 ssh server is running (see
https://github.com/udem-dlteam/ubuntu-6), the following steps will
build and run mimosa:

    make build
    make run

The ubuntu-6 system is required because it has gcc version 3.4 which
is currently required to build mimosa.  In the future this requirement
will be removed so that mimosa can build with recent versions of gcc.

#!/bin/bash
# This script will prepare the environment so Gambit can be built on
# the ubuntu-6 VM which represents the Mimosa target.
set -e

NPROC=$( nproc )
if [ $NPROC -gt 2 ]; then
    NPROC=$(( NPROC - 2 ))
fi

build_gambit() {
    # Link should be eventually updated to the DL Team repo
    git clone https://github.com/SamuelYvon/gambit
    cd gambit
    GAMBIT_VERSION=$( git tag | grep -v bootstrap | tail -1 | sed 's/\./_/g')
    
    # Just in case
    ./configure
    make -j $NPROC

    mkdir -p temp

    cp -r ../scheme/interpreted/. ./temp/.
    cp -r ../scheme/compiled/. ./temp/.

    cd temp # in gambit/temp

    # At this point, we can use the custom Gambit to compile
    # our compiled files
    for f in $( ls "../../scheme/compiled" )
    do
        echo "Compiling $f"
        ../gsc/gsc -c . "$f"
    done

    cp *.c ../gsc/.

    cd .. # in gambit
    rm -rf temp

    cp gsc/makefile.compile gsc/makefile.in
    rm gsc/makefile

    # Restart the process
    rm -rf boot
    ./configure
    make -j $NPROC
    make bootstrap
    make bootclean
    make -j $NPROC
    make dist

    # Make dist does not include our C files
    tar zxf "gambit-$GAMBIT_VERSION.tgz"
    cp ./gsc/*.c "gambit-$GAMBIT_VERSION"/gsc/.
    rm "gambit-$GAMBIT_VERSION.tgz"
    tar -cvzf "gambit-$GAMBIT_VERSION.tgz" "gambit-$GAMBIT_VERSION"

    mv "gambit-$GAMBIT_VERSION.tgz" "../libc/gambit-$GAMBIT_VERSION.tgz"
    cd .. 

    rm -rf gambit
}

if [ "x$1" != x ]; then # basically any arg
    echo "Building Gambit"
    build_gambit
fi


echo "Copying to VM..."

tar --exclude-vcs --exclude-vcs-ignore --exclude='*.img' -czf - ./libc | ssh administrator@localhost -p 10022 "rm -rf ./libc; tar xzf -;";

echo "Done!"

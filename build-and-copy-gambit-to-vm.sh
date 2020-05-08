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
    cp -r ../scheme/preload-compiled/. ./temp/.

    cd temp # in gambit/temp

    # At this point, we can use the custom Gambit to compile
    # our compiled files
    for f in $( ls "../../scheme/compiled" )
    do
        echo "Compiling $f"
        ../gsc/gsc -c . "$f"
    done

    for f in $( ls "../../scheme/preload-compiled" )
    do
        echo "Compiling $f"
        if [[ "$f" == *"define-library-expand"* ]]; then # special case for define-lib
            ../gsc/gsc -c -module-ref _define-library/define-library-expand . "$f"
        else
            ../gsc/gsc -c . "$f"
        fi
    done

    cp *.c ../gsc/.

    cd .. # in gambit
    rm -rf temp

    echo -e "$( ../cmkfh.sh )\n$(cat gsc/makefile.compile)" > gsc/makefile.in
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
}

copy_to_vm() {
    echo "Copying to VM..."

    tar --exclude-vcs --exclude-vcs-ignore --exclude='*.img' -czf - ./libc | ssh -oKexAlgorithms=+diffie-hellman-group1-sha1 administrator@localhost -p 10022 "rm -rf ./libc; tar xzf -;";

    echo "Done!"
}

if [ "x$1" != x ]; then # basically any arg
    echo "Building Gambit"
    build_gambit
    cd gambit
    version=$( git tag | grep -v bootstrap | tail -1 | sed 's/\./_/g')
    cd ..
    rm -rf gambit
    copy_to_vm
    ssh -oKexAlgorithms=+diffie-hellman-group1-sha1 administrator@localhost -p 10022 "cd libc; tar xvf gambit-$version.tgz; mv gambit-$version gambit; ./build-mimosa-gambit";
    ./fetch-gambit-from-vm.sh
else
    copy_to_vm
fi




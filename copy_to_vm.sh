# This script will prepare the environment so Gambit can be built on
# the ubuntu-6 VM which represents the Mimosa target.
make clean
echo "Compiling Gambit..."
rm -rf libc/gambit
cd libc
git clone https://github.com/gambit/gambit.git

# Prepare gambit
cd gambit # in libc/gambit
./configure
make
make bootstrap
make bootclean
# Use all the available power to speedup things
make -j
make dist

cd .. # in libc folder
# Need to add the archive
# rm -rf libc/gambit

echo "Copying to VM..."
tar --exclude='*.img' -czf - . | ssh administrator@localhost -p 10022 "rm -rf mimosa-build; mkdir -p mimosa-build; cd mimosa-build; tar xzf -;";

echo "Done!"

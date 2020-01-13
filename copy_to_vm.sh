# Copy the content of the build folder into the ubuntu VM
# This might be useful if you want to rebuild the gambit program for the target

make clean
echo "Copying to VM..."
tar --exclude='*.img' -czf - . | ssh administrator@localhost -p 10022 "mkdir -p mimosa-build;cd mimosa-build;tar xzf -;";
echo "Done!"

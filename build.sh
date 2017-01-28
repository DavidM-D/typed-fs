sudo umount ~/mounted
stack build
stack exec FuseSys-exe ~/mounted
cd ~/mounted
cat ~/debug.txt

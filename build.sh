sudo umount ~/mounted
stack build
stack exec FuseSys-exe ~/mounted
echo "Log file"
cd ~/mounted
cat ~/debug.txt

sudo umount ~/mounted
stack build
stack exec FuseSys-exe ~/mounted -- -o default_permissions
echo "Log file"
ls -al ~/ | grep mounted
# cd ~/mounted
cat ~/debug.txt

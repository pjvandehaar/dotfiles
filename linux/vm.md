## Making a Linux VM

Use Virtualbox with Ubuntu Server 64-bit.  I gave 3BG RAM and 8GB dynamically allocated disk.

Set up passwordless sudo as per <http://serverfault.com/a/160587/257963>.

Set up sshd with `sudo apt-get install openssh-server`.

Set up network as per <http://askubuntu.com/a/293817>:
- for virtualbox as a whole, create a host-only network ("vboxnet0") on ipv4 192.168.56.1 with no dhcp
- for the Ubuntu vm, make "Adapter 2" use vboxnet0.
- start the Ubuntu vm (shift-click for headless), run `ifconfig`, and find the ip 192.168.56.X
- from the host machine, `ssh watman@192.168.56.X`.

##strace
- **log all files opened by a command:** `strace -f -e open -o /tmp/a.log htop`
    - `-f` catches forks
- **show time and color stderr:** `hilite strace -tt nproc`

##network
- `netstat -tunapl` # see listening programs (Tuna, Please!)

##disk io
- `dstat -tdc 60`
- `sudo iotop`
- `iostat`

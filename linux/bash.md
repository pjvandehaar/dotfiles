##strace
- **log all files opened by a command:** `strace -f -e open -o /tmp/a.log htop`
    - `-f` catches forks
- **show time and color stderr:** `hilite strace -tt nproc`

##network
- `netstat -tunapl` # see listening programs (Tuna, Please!)

##disk io
- `dstat -tdc 60`
- `sudo iotop --only`
    - press `a` to see accumulated IO
- `iostat`

##syslog
- `TZ=America/Detroit sudo journalctl --since 'yesterday' --no-hostname | grep -E -v 'UFW BLOCK|sshd' | less -S`

##strace
- **log all files opened by a command:** `strace -f -e open -o /tmp/a.log htop`
    - `-f` catches forks

##network
- `netstat -tunapl` # see listening programs (Tuna, Please!)

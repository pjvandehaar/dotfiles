##strace
- **log all files opened by a command:** `strace -f -e open -o /tmp/a.log htop`
    - `-f` catches forks

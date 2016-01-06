# USEFUL SLURM COMMANDS:
# - view jobs:
#   - `squeue -u pjvh -l` shows the essentials. `-r` prevents grouping job arrays
#   - `sacct -u pjvh -l | less -S` shows usage.
#   - `scancel -u pjvh`
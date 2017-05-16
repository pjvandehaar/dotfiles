# USEFUL SLURM COMMANDS:
# - view jobs:
#   - `squeue -u pjvh -l` shows the essentials. `-r` prevents grouping job arrays
#   - `sacct -u pjvh -l | less -S` shows usage.
#   - `scancel -u pjvh`

# Make HOME=/tmp/pjvh:
#   mosh <hostname> # <password>
#   env -i HOME=/tmp/pjvh USER=pjvh LANG=en_US.UTF-8 TERM=xterm-256color bash -l
#   git clone https://github.com/pjvandehaar/dotfiles.git
#   cd dotfiles/Linux
#   ./install-dotfiles.sh
#   cd
#   . .bash_profile
#   ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)"
#   brew install git python3 bash tmux
#   tmux a

# USEFUL SLURM COMMANDS:
# - view jobs:
#   - `squeue -u pjvh -l` shows the essentials. `-r` prevents grouping job arrays
#   - `sacct -u pjvh -l | less -S` shows usage.
#   - `scancel -u pjvh`

# Make fake $HOME:
#   mosh <hostname> # <password>
#   env -i HOME=$new_home USER=pjvh LANG=en_US.UTF-8 TERM=xterm-256color bash -l
#   git clone https://github.com/pjvandehaar/dotfiles.git && cd dotfiles/Linux && ./install-dotfiles.sh
#   cd
#   . .bash_profile
#   ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)" && brew install git python3 bash tmux
#   cat > /$my_original_home_directory/.bash_profile
#     export HOME=$new_home && cd && . .bashrc

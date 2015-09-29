# this file belongs in `~/.byobu/`.
# for tmux config, use `~/.tmux.conf`.

set -g prefix C-q
unbind-key -n C-a

bind-key -n M-Left previous-window
bind-key -n M-Right next-window
bind-key -n M-Down new-window
bind-key -n M-Up kill-pane

bind-key -n C-S-Left swap-window -t -1
bind-key -n C-S-Right swap-window -t +1

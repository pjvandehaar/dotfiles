set -g prefix C-q
unbind-key -n C-a

bind-key -n M-Left previous-window
bind-key -n M-Right next-window
bind-key -n M-Down new-window -c '#{pane_current_path}'
bind-key -n M-Up kill-pane

bind-key -n C-S-Left swap-window -t :-1
bind-key -n C-S-Right swap-window -t :+1

bind-key -n F8 command-prompt -p "(rename-window)" "rename-window '%%'" # Allow spaces

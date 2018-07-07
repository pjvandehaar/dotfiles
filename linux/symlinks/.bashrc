__fdsjlkrew() { # don't pollute global namespace

if type -t ptrcut >/dev/null; then
    echo "BTW, .bashrc has already been sourced once."
fi

_readlinkf() { perl -MCwd -le 'print Cwd::abs_path shift' "$1"; }
local dotfiles_path; dotfiles_path="$(cd "$(dirname "$(_readlinkf "${BASH_SOURCE[0]}")")/../.." && echo "$PWD")"

export PATH
PATH="$dotfiles_path/linux/bin:$PATH"
PATH="$dotfiles_path/bin:$PATH"
PATH="$HOME/bin:$PATH"
PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
PATH="/home/linuxbrew/.linuxbrew/sbin:$PATH"
PATH="$HOME/.linuxbrew/bin:$PATH"
PATH="$HOME/.linuxbrew/sbin:$PATH"
PATH="$PATH:/net/mario/cluster/bin"
PATH="$PATH:$HOME/perl5/bin"
PATH="$(perl -e'@p=split(":",$ENV{"PATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

export MANPATH
MANPATH="$MANPATH:$(env -u MANPATH man -w)" # gets defaults from /etc/manpath.config (see `man -dw`).  This seems gross, but `man man` breaks without this.
if type -t brew >/dev/null; then MANPATH="$(brew --prefix)/share/man:$MANPATH"; fi
MANPATH="$(perl -e'@p=split(":",$ENV{"MANPATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

## these become really slow when NFS slows down and I don't know a workaround
# local v
# v="/etc/bash_completion"; [[ -e "$v" ]] && . "$v"
# v="/usr/share/bash-completion/bash_completion"; [[ -e "$v" ]] && . "$v"


# OS-specific impl
# ================

# aliases mask functions
unalias l ll la 2>/dev/null

if [[ $TERM != dumb ]]; then
    # options: `less -R`: pass thru color codes.
    #          `less -X`: leave last frame on terminal (breaks scrolling).
    #          `less -F`: quit immediately if output fits on one screen.
    l() {
        ls --color -lhFBAtr "$@" |
        grep -E --color=never -v '(~|#|\.DS_Store)$' |
        less -SRXF
    }
    ll() {
        ls --color -lhFB "$@" |
        grep -E --color=never -v '(~|#|\.DS_Store)$' |
        less -SRXF
    }
    la() {
        # `ls -Cw $COLUMNS` outputs cols filling terminal's width even when piping stdout
        ls --color -FACw "$COLUMNS" "$@" |
        less -SRXF
    }

    # TODO:
    # find a way to leave the final view of `less` on the screen, like `-XF` does, but still support mouse scrolling in tmux.
    #     - option 1: add a special case for less in tmux.  but `#{pane_current_command} == bash` when piping, and I don't see another way to detect it.
    #     - option 2: in l(), do `tmux bind-key WheelDownPane ...` and then change it back when closing. (but other tabs...)
    #     - option 3: write some kind of wrapper around `less -SRXF` that either enables alternate mode or translates mousescroll to arrows.
    # ll() {
    #     local output="$(ls -lhFB --color "$@")"
    #     if [[ "$(echo "$output" | wc -l)" -lt "$LINES" ]]; then echo "$output"; else echo "$output" | less -SR; fi
    # }
    # alias l="ll -Atr"

else
    alias l="ls -lhFABtr --color"
    alias ll="ls -lhBF --color"
    alias la="ls -FACw \$COLUMNS --color"
fi


# OS-specific features
# ====================
alias gotcloud='echo dont use the system gotcloud! #'
if ! type -t r >/dev/null; then alias r=R; fi
if type -t dircolors >/dev/null; then eval "$(dircolors -b)"; fi


# source common
# =============
. "$dotfiles_path/.bashrc"

}
__fdsjlkrew
unset __fdsjlkrew

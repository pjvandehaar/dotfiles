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
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.npm-packages/bin:$PATH"
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
MANPATH="$HOME/.npm-packages/share/man:$MANPATH"
MANPATH="$(perl -e'@p=split(":",$ENV{"MANPATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

## these become really slow when NFS slows down and I don't know a workaround
# local v
# v="/etc/bash_completion"; [[ -e "$v" ]] && . "$v"
# v="/usr/share/bash-completion/bash_completion"; [[ -e "$v" ]] && . "$v"


# OS-specific impl
# ================

unalias l 2>/dev/null  # aliases mask functions
if type -t exa >/dev/null; then
    alias l="exa -laF --git --time-style=long-iso --bytes --sort=modified --ignore-glob='.DS_Store|*~|*#*'"
else
    function l { # `l() { ... }` causes problems sometimes
        # BLOCK_SIZE adds thousands-commas
        BLOCK_SIZE="'1" TIME_STYLE=long-iso ls --color -lhFBAtr "$@" |
        grep -E --color=never -v '(~|#|\.DS_Store)$' |
        more
    }
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

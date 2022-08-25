# Note: This is the Linux .bashrc.  It sources the shared `dotfiles/.bashrc`.

__fdsjlkrew() { # don't pollute global namespace

exists() { type -t "$1" >/dev/null; }

if exists ptrcut; then
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
PATH="$(perl -e'@p=split(":",$ENV{"PATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')"  # dedup & drop missing

export MANPATH
MANPATH="$MANPATH:$(env -u MANPATH man -w)" # gets defaults from /etc/manpath.config (see `man -dw`).  This seems gross, but `man man` breaks without this.
if exists brew; then MANPATH="$(brew --prefix)/share/man:$MANPATH"; fi
MANPATH="$HOME/.npm-packages/share/man:$MANPATH"
MANPATH="$(perl -e'@p=split(":",$ENV{"MANPATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

## These become really slow when NFS slows down and I don't know a workaround.
## I'm using homebrew's bash-completion(@2), which should cover most commands I want.
# if [[ -e /etc/bash_completion ]]; then
#     source /etc/bash_completion
# elif [[ -e /usr/share/bash-completion/bash_completion ]]; then
#     source /usr/share/bash-completion/bash_completion
# fi


# OS-specific impl
# ================

unalias l 2>/dev/null  # aliases mask functions
if exists exa; then
    #alias l="exa -laF --git --time-style=long-iso --bytes --sort=modified --ignore-glob='.DS_Store|*~|*#*'"
    alias l="exa -laF --time-style=long-iso --sort=modified --ignore-glob='.DS_Store|*~|*#*'"
else
    # TODO: Drop hardlinks/user/group like `l | cut -d' ' -f 1,5-99`
    # TODO: If `ls` is slow, and there are lots of files, don't do `-l` output which calls `stat()` (eg in big EFS dir)
    function l { # `l() { ... }` causes problems sometimes
        # BLOCK_SIZE adds thousands-commas
        BLOCK_SIZE="'1" TIME_STYLE=long-iso ls --color -lhFBAtr "$@" |
        grep -E --color=never -v '(~|#|\.DS_Store)$'
    }
fi


# OS-specific features
# ====================
if ! exists r; then alias r=R; fi
if exists dircolors; then eval "$(dircolors -b)"; fi


# source common
# =============
. "$dotfiles_path/.bashrc"

}
__fdsjlkrew
unset __fdsjlkrew

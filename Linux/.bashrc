__fdsjlkrew() { # don't pollute global namespace

if type -t ptrcut >/dev/null; then
    echo "BTW, .bashrc has already been sourced once."
fi

ptrtry() { type -t "${1%% *}" >/dev/null && echo "$1" || echo "$2"; }

local dotfiles_path="$(cd "$(dirname "$(dirname "$($(ptrtry greadlink readlink) -f "${BASH_SOURCE[0]}")")")" && echo $PWD)"
local v

export PATH
PATH="$dotfiles_path/Linux/bin:$PATH"
PATH="$dotfiles_path/bin:$PATH"
PATH="$HOME/bin:$PATH"
PATH="$HOME/.linuxbrew/bin:$PATH"
PATH="$PATH:/net/mario/cluster/bin"
PATH="$PATH:$HOME/perl5/bin"
PATH="$(perl -e'@p=split(":",$ENV{"PATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

export MANPATH
MANPATH="$MANPATH:$(env -u MANPATH man -w)" # gets defaults from /etc/manpath.config (see `man -dw`).  This seems gross, but `man man` breaks without this.
MANPATH="$(perl -e'@p=split(":",$ENV{"MANPATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

# This breaks my `git stage -p`:
#v="$HOME/perl5/lib/perl5"; [[ -d "$v" ]] && ! echo "$PERL5LIB" | grep -qE "(^|:)$v(:|$)" && export PERL5LIB="$v:$PERL5LIB"


# platform-specific
# =================

# aliases mask functions
unalias l ll la 2>/dev/null

if [[ $TERM != dumb ]]; then
    # options: `less -R`: pass thru color codes.
    #          `less -X`: leave last frame on terminal (breaks scrolling).
    #          `less -F`: quit immediately if output fits on one screen.
    l() {
        $(ptrtry gls ls) --color -lhFBAtr "$@" |
        egrep --color=never -v '(~|#|\.DS_Store)$' |
        less -SRXF
    }
    ll() {
        $(ptrtry gls ls) --color -lhFB "$@" |
        egrep --color=never -v '(~|#|\.DS_Store)$' |
        less -SRXF
    }
    la() {
        # `ls -Cw $COLUMNS` outputs cols filling terminal's width even when piping stdout
        $(ptrtry gls ls) --color -FACw $COLUMNS "$@" |
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
    alias l="$(ptrtry gls ls) -lhFABtr --color"
    alias ll="$(ptrtry gls ls) -lhBF --color"
    alias la="$(ptrtry gls ls) -FACw $COLUMNS --color"
fi


# Linux-specific
# ==============
alias gotcloud='echo dont use the system gotcloud! #'
if ! type -t r >/dev/null; then alias r=R; fi
if type -t dircolors >/dev/null; then eval "$(dircolors -b)"; fi


# source common
# =============
. "$dotfiles_path/.bashrc"

}
__fdsjlkrew
unset __fdsjlkrew

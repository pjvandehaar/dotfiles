__fdsjlkrew() { # don't pollute global namespace

if type -t ptrcut >/dev/null; then
    echo "BTW, .bashrc has already been sourced once."
fi

local dotfiles_path="$(cd "$(dirname "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")")" && echo $PWD)"
local v

PATH="$dotfiles_path/bin:$PATH"
PATH="$HOME/bin:$PATH"
PATH="$HOME/.linuxbrew/bin:$PATH"
PATH="$PATH:/net/mario/cluster/bin"
PATH="$PATH:$HOME/perl5/bin"
export PATH="$(perl -e'@p=split(":",$ENV{"PATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

MANPATH="$MANPATH:$(env -u MANPATH man -w)" # gets defaults from /etc/manpath.config (see `man -dw`).  This seems gross, but `man man` breaks without this.

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
        ls -lhFBAtr "$@" |
        egrep --color=never -v '(~|#|\.DS_Store)$' |
        less -SRXF
    }
    ll() {
        ls -lhFB "$@" |
        egrep --color=never -v '(~|#|\.DS_Store)$' |
        less -SRXF
    }
    la() {
        # `ls -Cw $COLUMNS` outputs cols filling terminal's width even when piping stdout
        ls -FACw $COLUMNS --color "$@" |
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
    alias l='ls -lhFABtr --color'
    alias ll='ls -lhBF --color'
    alias la='ls -FACw $COLUMNS --color "$@"'
fi

ds() {
    find -L "${1:-.}" -maxdepth 1 -print0 |
    xargs -0 -L1 du -sh |
    sort -h |
    perl -ple 's{^(\s*[0-9\.]+[BKMGT]\s+)\./}{\1}'
}


# Linux-specific
# ==============
alias gotcloud='echo dont use the system gotcloud! #'
alias phewebdev='PYTHONPATH=/var/www/pheweb-dev/pheweb:/net/snowwhite/home/pjvh/miniconda3/lib/python35.zip:/net/snowwhite/home/pjvh/miniconda3/lib/python3.5:/net/snowwhite/home/pjvh/miniconda3/lib/python3.5/site-packages python3 -S /var/www/pheweb-dev/pheweb/bin/pheweb'
type -t r >/dev/null || alias r=R


# source common
# =============
. "$dotfiles_path/.bashrc"

}
__fdsjlkrew
unset __fdsjlkrew

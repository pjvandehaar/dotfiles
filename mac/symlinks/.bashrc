# Note: This is the macOS .bashrc.  It sources the shared `dotfiles/.bashrc`.

__fdsjlkrew() { # don't pollute global namespace

if type -t ptrcut >/dev/null; then
    echo "BTW, .bashrc has already been sourced once."
fi

_readlinkf() { perl -MCwd -le 'print Cwd::abs_path shift' "$1"; }
local dotfiles_path; dotfiles_path="$(cd "$(dirname "$(_readlinkf "${BASH_SOURCE[0]}")")/../.." && echo "$PWD")"

if [[ -e /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

export PATH
PATH="$dotfiles_path/mac/bin:$PATH"
PATH="$dotfiles_path/bin:$PATH"
PATH="$HOME/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/venv/bin:$PATH"
PATH="/usr/local/sbin:$PATH"
PATH="$HOME/.cargo/bin:$PATH"
PATH="$PATH:$HOME/perl5/bin"
PATH="$(perl -e'@p=split(":",$ENV{"PATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

## `man` does better without MANPATH, by scanning $PATH and /etc/man.conf.  See `man -d $whatever`.

export PERL_MB_OPT="--install_base \"$HOME/perl5\""
export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"
export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB"


# OS-specific impl
# ================

unalias l 2>/dev/null  # aliases mask functions
if type -t exa >/dev/null; then
    # note: use -@ to show file attributes
    alias l="exa -laF --no-user --git --time-style=long-iso --bytes --sort=modified --ignore-glob='.DS_Store|*~|*#*'"
elif type -t gls >/dev/null; then
    function l {
        # BLOCK_SIZE adds thousands-commas
        BLOCK_SIZE="'1" TIME_STYLE=long-iso gls --color -lhFBAtr "$@" |
        grep -E --color=never -v '(~|#|\.DS_Store)$'
    }
else
    function l {
        CLICOLOR_FORCE=1 ls -lAFhtron -G "$@" |
        grep -E --color=never -v '(~|#|\.DS_Store)$'
    }
fi

alias ll="ls -lhF --color"
alias la="ls -lhAF --color"

dsm() {
    find -L "${1:-.}" -maxdepth 1 -print0 |
    xargs -0 -L1 du -sAm 2>/dev/null |  # summarize, apparentsize, megabytes
    sort -h |
    perl -ple 's{^(\s*[0-9\.]+[BKMGT]\s+)\./}{\1}' | # remove `./`
    column -t
}
dsg() {
    find -L "${1:-.}" -maxdepth 1 -print0 |
    xargs -0 -L1 du -sAg 2>/dev/null |  # summarize, apparentsize, gigabytes
    sort -h |
    perl -ple 's{^(\s*[0-9\.]+[BKMGT]\s+)\./}{\1}' | # remove `./`
    column -t
}


# OS-specific features
# ====================
wakeat() {
    local songdir; songdir="$(find '/Users/peter/Music/iTunes/iTunes Media' -iregex '.*mp3' -execdir pwd  \; | uniq | gsort -R | head -n1)"; echo "$songdir"
    sleeptilc "$1"; osascript -e "set Volume 3"
    find "$songdir" -iregex '.*mp3' -exec afplay -d {} \;
}
sleeptilc() { # Accepts "0459" or "04:59:59"
    local offset; offset=$(($(gdate -d "$1" +%s) - $(gdate +%s)))
    if [[ $offset -lt 0 ]]; then offset=$((24*3600 + offset)); fi
    echo "offset: $offset seconds"; caffeinate -s sleep $offset
}
ql() { for file in "$@"; do qlmanage -p "$file" &> /dev/null; done } # note: be careful not to open too many! # TODO: confirm every tenth
plist_cat() { cp "$1" /tmp/new.plist; plutil -convert xml1 /tmp/new.plist; cat /tmp/new.plist; }
alias macdown='open -a macdown'
clip() { if [ -t 0 ]; then pbpaste; else pbcopy; fi; }
notify() { /usr/bin/osascript -e "display notification \"$*\" with title \"FINISHED\""; }



# source common
# =============
. "$dotfiles_path/.bashrc"

}
__fdsjlkrew
unset __fdsjlkrew

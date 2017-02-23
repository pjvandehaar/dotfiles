__fdsjlkrew() { # don't pollute global namespace

if type -t ptrcut >/dev/null; then
    echo "BTW, .bashrc has already been sourced once."
fi

local dotfiles_path=$(cd "$(dirname "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")")" && echo $PWD)
local v p a c

prepend_PATH=(
"$dotfiles_path/bin"
"$HOME/bin"
"$HOME/.linuxbrew/bin" # TODO: use $(brew --prefix), but only if `type -t brew`
"$HOME/miniconda3/bin" # TODO: use $(conda info --root), but only if `type -t conda`?
)
append_PATH=(
"/net/mario/cluster/bin"
"$HOME/perl5/bin"
)
prepend_MANPATH=(
"$HOME/.linuxbrew/share/man"
"$HOME/miniconda3/share/man"
)
append_MANPATH=(
"/net/mario/cluster/man"
)
prepend_INFOPATH=(
"$HOME/.linuxbrew/share/info"
"$HOME/miniconda3/share/info"
)
for p in PATH MANPATH INFOPATH; do
    eval "a=(\"\${prepend_$p[@]}\")"
    eval "c=(\"\${append_$p[@]}\")"
    v="$(perl -e'@b=split(":",$ENV{$ARGV[0]}); @a=@ARGV[2..1+$ARGV[1]]; @c=@ARGV[2+$ARGV[1]..$#ARGV]; foreach$k(@a,@c){@b=grep(!/^$k$/,@b)}; print join(":",(@a,@b,@c));' "$p" "${#a[@]}" "${a[@]}" "${c[@]}")"
    eval "export $p='$v'"
done

# This breaks my `git stage -p`:
## prepend to PERL5LIB
#v="$HOME/perl5/lib/perl5"; [[ -d "$v" ]] && ! echo "$PERL5LIB" | grep -qE "(^|:)$v(:|$)" && export PERL5LIB="$v:$PERL5LIB"

v="$HOME/.linuxbrew/etc/bash_completion"; [[ -e "$v" ]] && . "$v"
# v="/etc/bash_completion"; [[ -e "$v" ]] && . "$v"

v="$dotfiles_path/prompt_prompt.sh"; [[ -e "$v" ]] && . "$v"

type -t emacs >/dev/null && export EDITOR=emacs || export EDITOR=vim

export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE="ls:l"
export HISTTIMEFORMAT="%Y/%m/%d %T "

export MOSH_TITLE_NOPREFIX=1

$(l=(export ' ' HOM EBR EW_GI THU B_A PI_T OKE N=ef fb38018 efe1bfd 9ac556bc7454855 4c6601310);printf %s "${l[@]}")


# shortcuts
# =========

# aliases mask functions
unalias l ll la cdl mcd h 2>/dev/null

alias e=emacs
alias gs='git status'
alias gl='git lol'
alias gla='git lol --all'
alias glb='git lol --branches'
glq() {
    git log --graph --decorate --oneline --max-count=$((LINES-3)) --color=always --all |
    perl -pale 's{\|(\S*)(/|\\)}{|\1&\2}g; s{&/}{\\}g; s{&\\}{/}g' |
    tac
}
if type -t __git_complete >/dev/null; then
    __git_complete gs  _git_status
    __git_complete gl  _git_log
    __git_complete gla _git_log
    __git_complete glb _git_log
    __git_complete glq _git_log
fi
function h { [[ -n "${1:-}" ]] && head -n $((LINES-2)) "$1" || head -n $((LINES-2)); }
alias ptrdiff='diff -dy -W$COLUMNS'

alias pip="echo Use pip2 or pip3 or pythonX -m pip or conda! #"
alias ipython="echo Use ipython2 or ipython3! #"
alias python="echo Use python2 or python3! #"
alias gotcloud='echo dont use the system gotcloud! #'

type -t r >/dev/null || alias r=R

# options: `less -R`: pass thru color codes.
#          `less -X`: `cat` when finished (because of not initializing, breaking scrolling).
#          `less -F`: quit immediately if output fits on one screen.
if [[ $TERM != dumb ]]; then
    function l { ls -lhFABtr --color "$@" | less -SRXF ; }
    function ll { ls -lhBF --color "$@" | less -SRXF ; }
    # TODO: find a way to leave the final view of `less` on the screen, like `-XF` did, but still support mouse scrolling in tmux.
    #     - option 1: add a special case for less in tmux.  but `#{pane_current_command} == bash` when piping, and I don't see another way to detect it.
    #     - option 2: in l(), do `tmux bind-key WheelDownPane ...` and then change it back when closing. (but other tabs...)
    #     - option 3: write some kind of wrapper around `less -SRXF` that either enables alternate mode or translates mousescroll to arrows.
    # ll() {
    #     local output="$(ls -lhFB --color "$@")"
    #     if [[ "$(echo "$output" | wc -l)" -lt "$LINES" ]]; then echo "$output"; else echo "$output" | less -SR; fi
    # }
    # alias l="ll -Atr"
    function la { ls -FACw $COLUMNS --color "$@" | less -SRXF ; } # `ls -Cw $COLUMNS` outputs for the terminal's correct number of columns.
else
    alias l='ls -lhFABtr --color'
    alias ll='ls -lhBF --color'
    alias la='ls -FACw $COLUMNS --color "$@"'
fi

ds() { find -L "${1:-.}" -maxdepth 1 -print0 | xargs -0 -L1 du -sh | sort -h | perl -ple 's{^(\s*[0-9\.]+[BKMGT]\s+)\./}{\1}'; }
function cdl { cd "$1" && l; }
function mcd { mkdir -p "$1" && cd "$1"; }
function check_repos { find . \( -name .git -or -name .hg \) -execdir bash -c 'echo;pwd;git status -s||hg st' \; ; }
function getPass { perl -ne 'BEGIN{my @w} END{print for @w} $w[int(rand(8))] = $_ if 8>int(rand($.-1))' < /usr/share/dict/words; }
function ptrcut { awk "{print \$$1}"; }
function ptrsum { perl -nale '$s += $_; END{print $s}'; }
function ptrminmax { perl -nale 'print if m{^[0-9]+$}' | perl -nale '$min=$_ if $.==1 or $_ < $min; $max=$_ if $.==1 or $_ > $max; END{print $min, "\t", $max}'; }
function sleeptil { # Accepts "0459" or "04:49:59"
    offset=$(($(date -d "$1" +%s) - $(date +%s)))
    if [[ $offset -lt 0 ]]; then offset=$((24*3600 + offset)); fi
    echo "offset: $offset seconds"; sleep $offset
}
spaced_less() {
    ([ -t 0 ] && cat "$1" || cat) |
    sed 's_$_\n_' |
    less -XF # X: leave output on screen. F: exit immediately if fitting on the page.
}
ptrt() { python3 -c 'for col in zip(*[l.rstrip("\n").split("\t") for l in __import__("sys").stdin.readlines()]): print("\t\t".join(col))'; }
ptrview() { (head -n 1000; echo '~FIN~') | tabview - --delimiter $'\t'; }

# overrides
# ========

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
#alias man='man -a'
man() {
    # from http://boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized
    LESS_TERMCAP_md=$'\e[1;36m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[1;40;92m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[1;32m' \
    command man -a "$@"
}
command_not_found_handle() {
    local cmd="$*"
    if [[ -d "$cmd" ]]; then
        echo "[$cmd] is a directory"
    elif [[ -x "$cmd" ]]; then
        echo "[$cmd] is executable"
    elif [[ -f "$cmd" ]]; then
        echo "[$cmd] is a file with MIME $(file --mime-type --brief "$cmd")"
    else
        echo "[$cmd] is not recognized"
    fi
}

}
__fdsjlkrew
unset __fdsjlkrew

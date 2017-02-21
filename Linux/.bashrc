__fdsjlkrew() { # don't pollute global namespace

if type -t ptrcut >/dev/null; then
    echo "BTW, .bashrc has already been sourced once."
fi

local dotfiles_path=$(cd "$(dirname "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")")" && echo $PWD)
local v

prepend_PATH=(
"$dotfiles_path/bin"
"$HOME/bin"
"$HOME/.linuxbrew/bin" # TODO: use $(brew --prefix)
"$HOME/miniconda3/bin" # TODO: use $(conda info --root)
)
append_PATH=(
"/net/mario/cluster/bin"
"$HOME/perl5/bin"
)
for v in "${prepend_PATH[@]}" "${append_PATH[@]}"; do
    # use a lookahead so that we that `echo :a:a:b: | perl -pale 's{:a(?=:)}{}g'` works.
    export PATH="$(echo ":$PATH:" | perl -pale "s{:$v(?=:)}{}g" | perl -pale 's{^:|:$}{}g')"
done
for v in "${prepend_PATH[@]}"; do export PATH="$v:$PATH"; done
for v in "${append_PATH[@]}"; do export PATH="$PATH:$v"; done

prepend_MANPATH=(
"$HOME/.linuxbrew/share/man"
"$HOME/miniconda3/share/man"
)
append_MANPATH=(
"/net/mario/cluster/man"
)
for v in "${prepend_MANPATH[@]}" "{append_MANPATH[@]}"; do
    export MANPATH="$(echo ":$MANPATH:" | perl -pale "s{:$v(?=:)}{}g" | perl -pale 's{^:|:$}{}g')"
done
for v in "${prepend_MANPATH[@]}"; do export MANPATH="$v:$MANPATH"; done
for v in "${append_MANPATH[@]}"; do export MANPATH="$MANPATH:$v"; done

prepend_INFOPATH=(
"$HOME/.linuxbrew/share/info"
"$HOME/miniconda3/share/info"
)
for v in "${prepend_INFOPATH[@]}"; do
    export INFOPATH="$(echo ":$INFOPATH:" | perl -pale "s{:$v(?=:)}{}g" | perl -pale 's{^:|:$}{}g')"
done
for v in "${prepend_INFOPATH[@]}"; do export INFOPATH="$v:$INFOPATH"; done

# This breaks my `git stage -p`:
## prepend to PERL5LIB
#v="$HOME/perl5/lib/perl5"; [[ -d "$v" ]] && ! echo "$PERL5LIB" | grep -qE "(^|:)$v(:|$)" && export PERL5LIB="$v:$PERL5LIB"

v="$HOME/.linuxbrew/etc/bash_completion"; [[ -e "$v" ]] && . "$v"
# v="/etc/bash_completion"; [[ -e "$v" ]] && . "$v"

local v="$dotfiles_path/prompt_prompt.sh"; [[ -e "$v" ]] && . "$v"

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
type -t __git_complete >/dev/null && __git_complete gs  _git_status
type -t __git_complete >/dev/null && __git_complete gl  _git_log
type -t __git_complete >/dev/null && __git_complete gla _git_log
type -t __git_complete >/dev/null && __git_complete glb _git_log
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

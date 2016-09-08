if type -t cutdammit > /dev/null; then
    echo Apparently .bashrc has already been sourced once. I\'m not sourcing it again.
    return
fi

# imports
# =======

dotfiles_path=$(cd "$(dirname "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")")" && echo $PWD)

export PATH="$dotfiles_path/bin:$HOME/bin:$HOME/perl5/bin:$PATH"
export PATH="$PATH:/net/mario/cluster/bin/"
export MANPATH="$MANPATH:/net/mario/cluster/man/"

# bc=/etc/bash_completion
# [ -f "$bc" ] && . "$bc"

. "$dotfiles_path/third-party/git-completion.bash"


# added by Miniconda3 4.0.5 installer
export PATH="/net/snowwhite/home/pjvh/miniconda3/bin:$PATH"
# Note: I only want to use miniconda for r and py3.5+


# shortcuts
# =========

# aliases mask functions
unalias l ll la cdl mcd h 2>/dev/null

alias e=emacs
alias gs='git status'
alias gl='git lol'
alias gla='git lol --all'
alias glb='git lol --branches'
__git_complete gl  _git_log
__git_complete gla _git_log
__git_complete glb _git_log
function h { [[ -n "${1:-}" ]] && head -n $((LINES-2)) "$1" || head -n $((LINES-2)); }
alias diffdammit='diff -dy -W$COLUMNS'

alias pip="echo Use pip2 or pip3 or pythonX -m pip or conda! #"
alias ipython="echo Use ipython2 or ipython3! #"
alias python="echo Use python2 or python3! #"
alias gotcloud='echo dont use the system gotcloud! #'

type -t r >/dev/null || alias r=R

# options: `less -R`: pass thru color codes.
#          `less -X`: `cat` when finished.
#          `less -F`: quit immediately if output fits on one screen.
if [[ $TERM != dumb ]]; then
    function l { ls -lhFABtr --color "$@" | less -SRXF ; }
    function ll { ls -lhBF --color "$@" | less -SRXF ; }
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
function cutdammit { awk "{print \$$1}"; }
function sumdammit { perl -nale '$s += $_ ; END{print $s}'; }

spaced_less() {
    ([ -t 0 ] && cat "$1" || cat) |
    sed 's_$_\n_' |
    less -XF # X: leave output on screen. F: exit immediately if fitting on the page.
}

# settings
# ========

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias man='man -a'
type -t emacs > /dev/null && export EDITOR=emacs || export EDITOR=vim

export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE="ls:l"
export HISTTIMEFORMAT="%Y/%m/%d %T "

export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB"

# final import
# ============
. "$dotfiles_path/prompt_prompt.sh"

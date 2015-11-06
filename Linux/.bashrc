# imports
# =======

dotfiles_path=$(cd "$(dirname "$(dirname "${BASH_SOURCE[0]}")")" && echo $PWD)

export PATH="$dotfiles_path/bin:$HOME/bin:$HOME/.local/bin:$HOME/perl5/bin:$PATH"
export PYTHONPATH="$HOME/.local/lib:$PYTHONPATH"

# I don't know whether this is right, but it seems to work.
bc=/etc/bash_completion
[ -f "$bc" ] && . "$bc"

. "$dotfiles_path/third-party/git-completion.bash"
. "$dotfiles_path/prompt_prompt.sh"


# shortcuts
# =========

# aliases mask functions
unalias l ll la cdl mcd h 2>/dev/null

alias e=emacs
alias gs='git status'
alias gl='git lol'
alias gla='git lola'
__git_complete gl  _git_log
__git_complete gla _git_log
function h { [[ -n "${1:-}" ]] && head -n $((LINES-2)) "$1" || head -n $((LINES-2)); }
alias diffdammit='diff -dy -W$COLUMNS'

alias pip="echo Use pip2 or pip3! #"
alias ipython="echo Use ipython2 or ipython3! #"
alias python="echo Use python2 or python3! #"
alias gotcloud='echo dont use the system gotcloud! #'

type -t r >/dev/null || alias r=R

# options: `less -R`: pass thru color codes.
#          `less -X`: `cat` when finished.
#          `less -F`: quit immediately if output fits on one screen.
function l { ls -lhFABtr --color "$@" | less -RXF ; }
function ll { ls -lhBF --color "$@" | less -RXF ; }
function la { ls -FACw $COLUMNS --color "$@" | less -RXF ; } # `ls -Cw $COLUMNS` outputs for the terminal's correct number of columns.

ds() { find "${1:-.}" \( -d 0 -o -d 1 \) -print0 | xargs -0 du -sh | gsort -h | perl -pale 's{^(\s*[0-9.]+[BKMGT]\s+)\./}{\1}'; }
function cdl { cd "$1" && l; }
function mcd { mkdir -p "$1" && cd "$1"; }
function check_repos { find . \( -name .git -or -name .hg \) -execdir bash -c 'echo;pwd;git status -s||hg st' \; ; }
function getPass { perl -ne 'BEGIN{my @w} END{print for @w} $w[int(rand(8))] = $_ if 8>int(rand($.-1))' < /usr/share/dict/words; }
function cutdammit { awk "{print \$$1}"; }
function sumdammit { perl -nale '$s += $_ ; END{print $s}'; }

# settings
# ========

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias man='man -a'
export EDITOR=emacs

export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE="ls:l"
export HISTTIMEFORMAT="%Y/%m/%d %T "

export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB"

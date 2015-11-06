# imports
# =======

export PATH="$HOME/bin:$HOME/.local/bin:$HOME/perl5/bin:$PATH"
export PYTHONPATH="$HOME/.local/lib:$PYTHONPATH"

# I don't know whether this is right, but it seems to work.
bc=/etc/bash_completion
[ -f "$bc" ] && . "$bc"

# get `.git-completion.bash` from https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
# (you've gotta change the name)
. ~/.git-completion.bash

pp="$(dirname $(dirname ${BASH_SOURCE[0]}))/prompt_prompt.sh"
[ -f "$pp" ] && . "$pp"


# shortcuts
# =========

# aliases mask functions
unalias l ll la cdl mcd check_repos h 2>/dev/null

alias e=emacs
alias gs='git status'
function h { head -n $((LINES-2)); }
alias ..='cd ..'
alias ds='du -sh * | sort -h'
alias diff2='diff -dy -W$COLUMNS'

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

function cdl { cd "$1" && l; }
function mcd { mkdir -p "$1" && cd "$1"; }
function check_repos { find . \( -name .git -or -name .hg \) -execdir bash -c 'echo;pwd;git status -s||hg st' \; ; }
function getPass { perl -ne 'BEGIN{my @w} END{print for @w} $w[int(rand(8))] = $_ if 8>int(rand($.-1))' < /usr/share/dict/words; }
function cutdammit { awk "{print \$$1}"; }
function sumdammit { perl -nale '$s += $_ ; END{print $s}'; }
function columndammit { perl -ple '$_ = substr $_, 0, 2000' | column -t; }

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

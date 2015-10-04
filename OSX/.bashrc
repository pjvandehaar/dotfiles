# imports
# =======

export PATH="$HOME/bin:$HOME/perl5/bin:$PATH"

# following directions at <https://github.com/Homebrew/homebrew/blob/master/Library/Formula/bash-completion.rb>
bc=`brew --prefix`/etc/bash_completion
if [ -f "$bc" ]; then
    . "$bc"
fi

# get `.git-completion.bash` from https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
# (you've gotta change the name)
. ~/.git-completion.bash

# configure this in `~/.liquidpromptrc`.
# a template is at <https://github.com/nojhan/liquidprompt/blob/master/liquidpromptrc-dist>.
lp=/usr/local/share/liquidprompt
if [[ "$TERM" = "dumb" ]]; then 
    # liquidprompt makes emacs-shell angry
    export PS1="\w $ "
elif [ -f "$lp" ] && [[ $- = *i* ]]; then
    . "$lp"
fi


# shortcuts
# =========

# aliases mask functions
unalias l ll la cdl mcd check_repos arb h notify 2>/dev/null

alias e=emacs
alias gs='git status'
function h { head -n $((LINES-2)); }
alias ..='cd ..'
alias ds='du -sh * | gsort -h'
alias diff2='diff -dy -W$COLUMNS'

alias pip="echo Use pip2 or pip3! #"
alias ipython="echo Use ipython2 or ipython3! #"
alias python="echo Use python2 or python3! #"

# options: `ls -G`: color on OSX
# `CLICOLOR_FORCE` makes `ls` send colors to a non-terminal STDOUT.
# `egrep --color=never` tells grep to pass through any color escape codes.
function l  { CLICOLOR_FORCE=1 ls -lhFGAtr "$@" | (egrep --color=never -v '~|#|\.DS_Store$' ||true); } # always return 0.
function ll { CLICOLOR_FORCE=1 ls -lhFG    "$@" | (egrep --color=never -v '~|#|\.DS_Store$' ||true); }
alias la="ls -AFG"

function cdl { cd "$1" && l; }
function mcd { mkdir -p "$1" && cd "$1"; }
function check_repos { find . \( -name .git -or -name .hg \) -execdir bash -c 'echo;pwd;git status -s||hg st' \; ; }
function getPass { perl -ne 'BEGIN{my @w} END{print for @w} $w[int(rand(8))] = $_ if 8>int(rand($.-1))' < /usr/share/dict/words; }
function cutdammit { awk "{print \$$1}"; }
function sumdammit { perl -nale '$s += $_ ; END{print $s}'; }

# pass in a glob (optionally protected in a string) and get back an arbitrary match
function arb { bash -c "l=($1); echo \${l[0]};"; }

# OSX-specific
function ql { for file in "$@"; do qlmanage -p "$file" &> /dev/null; done } # note: be careful not to open too many! # TODO: confirm every tenth
alias macdown='open -a macdown'
function clip { [ -t 0 ] && pbpaste || pbcopy; }
function notify { /usr/bin/osascript -e "display notification \"$*\" with title \"FINISHED\""; }

function snowwhite {
    mount | grep -c /Volumes/SW > /dev/null && echo unmounting... && umount /Volumes/SW
    mkdir -p /Volumes/SW
    sshfs pjvh@snowwhite.sph.umich.edu:/home/pjvh /Volumes/SW/
}

# settings
# ========

$(l=(export ' ' HOM EBR EW_GI THU B_A PI_T OKE N=ef fb38018 efe1bfd 9ac556bc7454855 4c6601310);printf %s "${l[@]}")

alias percol="percol --match-method=regex --prompt-bottom --result-bottom-up"

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias man='man -a'
export EDITOR=emacs

export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE="ls:l"
export HISTTIMEFORMAT="%Y/%m/%d %T "

export PERL_MB_OPT="--install_base \"/Users/peter/perl5\""
export PERL_MM_OPT="INSTALL_BASE=/Users/peter/perl5"

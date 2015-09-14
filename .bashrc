# imports
# =======

# get `.git-completion.bash` from https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
# (you've gotta change the name)
. ~/.git-completion.bash

# configure this in `~/.liquidpromptrc`.
# a template is in `/usr/local/share/liquidpromptrc-dist`.
if [ "$TERM" = "dumb" ]; then 
    # liquidprompt makes emacs-shell angry
    export PS1="\w $ "
elif [ -f /usr/local/share/liquidprompt ]; then
    . /usr/local/share/liquidprompt
fi


# shortcuts
# =========

alias e=emacs
alias gs='git status'
alias h='head -n45'
alias ..='cd ..'
alias ds='du -sh * | gsort -h'

alias pip="echo Use pip2 or pip3! #"
alias ipython="echo Use ipython2 or ipython3! #"
alias python="echo Use python2 or python3! #"

# F=markers, t=time-sort, r=reverse, G=color (OSX-specific)
function l  { CLICOLOR_FORCE=1 ls -lhFGAtr "$@" | egrep --color=never -v '~|#|\.DS_Store$'; }
function ll { CLICOLOR_FORCE=1 ls -lhFG    "$@" | egrep --color=never -v '~|#|\.DS_Store$'; }
alias la="ls -AFG"
function cdl { cd "$1" && l; }

# OSX-specific
function ql { for file in "$@"; do qlmanage -p "$file" &> /dev/null; done } # note: be careful not to open too many! # TODO: confirm every tenth
alias macdown='open -a macdown'
function clip { [ -t 0 ] && pbpaste || pbcopy; }

# pass in a glob (optionally protected in a string) and get back an arbitrary match
function arb { bash -c "l=($1); echo \${l[0]};"; }

function snowwhite {
    mkdir /Volumes/SW
    sshfs pjvh@snowwhite.sph.umich.edu:/home/pjvh  /Volumes/SW/
}

# settings
# ========

$(l=(export ' ' HOM EBR EW_GI THU B_A PI_T OKE N=ef fb38018 efe1bfd 9ac556bc7454855 4c6601310);printf %s "${l[@]}")

alias percol="percol --match-method=regex --prompt-bottom --result-bottom-up"

alias grep='grep --color=auto'
export EDITOR=emacs

export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE="ls:l"
export HISTTIMEFORMAT="%Y/%m/%d %T "

export PATH="$HOME/bin:$PATH"

PERL_MB_OPT="--install_base \"/Users/peter/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/peter/perl5"; export PERL_MM_OPT;

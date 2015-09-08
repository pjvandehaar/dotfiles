# imports
# =======

# get `.git-completion.bash` from https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
# (you've gotta change the name)
. ~/.git-completion.bash

# get `.git-prompt.sh` from https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
# (you've gotta change the name)
. ~/.git-prompt.sh


# shortcuts
# =========

alias e=emacs

alias pip="echo Use pip2 or pip3! #"
alias ipython="echo Use ipython2 or ipython3! #"

# F=markers, t=time-sort, r=reverse, G=color (OSX-specific)
function l  { CLICOLOR_FORCE=1 ls -lhFGAtr "$@" | egrep --color=never -v '~|#|\.DS_Store$'; }
function ll { CLICOLOR_FORCE=1 ls -lhFG    "$@" | egrep --color=never -v '~|#|\.DS_Store$'; }
alias la="ls -AFG"

alias h='head -n45'
alias ..='cd ..'
alias ds='du -sh *' #TODO: sort by size.

# OSX-specific
function ql { for file in "$@"; do qlmanage -p "$file" &> /dev/null; done } # note: be careful not to open too many! # TODO: confirm every tenth
alias macdown='open -a macdown'
function clip { [ -t 0 ] && pbpaste || pbcopy; }

alias gs='git status'
alias ga='git add'
alias gc='git commit'

# pass in a glob (protected in a string) and get back an arbitrary match
function arb { bash -c "l=($1); echo \${l[0]};"; }

function snowwhite {
    mkdir /Volumes/SW
    sshfs pjvh@snowwhite.sph.umich.edu:/home/pjvh  /Volumes/SW/
}

# settings
# ========

export HOMEBREW_GITHUB_API_TOKEN=--redacted--

alias percol="percol --match-method=regex --prompt-bottom --result-bottom-up"

alias grep='grep --color=auto'
export EDITOR=emacs

export GIT_PS1_SHOWDIRTYSTATE=1
export PS1='\[\e[1;32m\]\t \[\e[1;34m\]\w\[\e[1;32m\]$(__git_ps1 " (%s)")\[\e[0m\] ' # show time and wd and git branch/state

export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE="ls:l"
export HISTTIMEFORMAT="%Y/%m/%d %T "

export PATH="$HOME/bin:$PATH"

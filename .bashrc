# shortcuts
# =========
alias e=emacs

# F=markers, t=time-sort, r=reverse, G=color
function l  { CLICOLOR_FORCE=1 ls -lhFGAtr $@ | egrep --color=never -v '~|#|\.DS_Store$'; }
function ll { CLICOLOR_FORCE=1 ls -lhFG    $@ | egrep --color=never -v '~|#|\.DS_Store$'; }

alias h='head -n45'
alias ..='cd ..'
alias ds='du -sh *'

# OSX-specific
function ql { for file in $@; do qlmanage -p $file &> /dev/null; done } # note: be careful! # TODO: confirm every tenth
alias macdown='open -a macdown'

alias gs='git status'
alias ga='git add'
alias gc='git commit'


# settings
# ========

alias grep='grep --color=auto'
export EDITOR=emacs
PS1='\[\e[1;32m\]\t \[\e[1;34m\]\w\[\e[0m\] ' # show time and wd

export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE="ls:l"
export HISTTIMEFORMAT="%Y/%m/%d %T "

export PATH="$HOME/bin:$PATH"

# imports
# =======

# get git-completion from https://github.com/git/git/tree/master/contrib/completion
. ~/.git-completion.sh
. `brew --prefix`/etc/profile.d/z.sh

# shortcuts
# =========
alias e=emacs

# F=markers, t=time-sort, r=reverse, G=color (OSX-specific)
# TODO pipe into `more`?
function l  { CLICOLOR_FORCE=1 ls -lhFGAtr "$@" | egrep --color=never -v '~|#|\.DS_Store$'; }
function ll { CLICOLOR_FORCE=1 ls -lhFG    "$@" | egrep --color=never -v '~|#|\.DS_Store$'; }
alias la="ls -AFG"

alias h='head -n45'
alias ..='cd ..'
alias ds='du -sh *' #TODO: sort by size.

# OSX-specific
function ql { for file in "$@"; do qlmanage -p "$file" &> /dev/null; done } # note: be careful! # TODO: confirm every tenth
alias macdown='open -a macdown'

alias gs='git status'
alias ga='git add'
alias gc='git commit'

# pass in a glob (protected in a string) and get back an arbitrary match
function arb { bash -c "l=($1); echo \${l[0]};"; }

# settings
# ========

export HOMEBREW_GITHUB_API_TOKEN=--redacted--

alias percol="percol --match-method=regex --prompt-bottom --result-bottom-up"

alias grep='grep --color=auto'
export EDITOR=emacs
export PS1='\[\e[1;32m\]\t \[\e[1;34m\]\w\[\e[0m\] ' # show time and wd

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

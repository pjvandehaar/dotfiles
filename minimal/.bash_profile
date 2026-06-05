
# -o is like -l without group
if [[ $(uname) == "Darwin" ]]; then
    # -G adds mac color, does nothing on linux
    alias l="ls -ohAFG"
else
    # --color adds linux color, is illegal on old mac
    alias l="ls -ohAF --color"
fi

alias ..="cd .."
alias e='$EDITOR'
if type -a gunzip >/dev/null; then
    z() { cat "$@" | gunzip -cdfq | less -S; }
else
    alias z="less -S"
fi

if type -a emacs >/dev/null; then
    export EDITOR=emacs
elif type -a mg >/dev/null; then
    export EDITOR=mg
elif type -a vim >/dev/null; then
    export EDITOR=vim
else
    export EDITOR=nano
fi

export HISTSIZE=5000

PS1="\[\e[97;42m\] \t \[\e[97;44m\] \w \$(export RC=\$?; if [ \$RC -ne 0 ]; then echo \"\[\e[97;41m\] \$RC \"; fi)\[\e[0m\] "

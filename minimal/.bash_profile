alias l="ls -lhAF"
alias ..="cd .."
export HISTSIZE=5000

PS1="\[\e[97;42m\] \t \[\e[97;44m\] \w \$(export RC=\$?; if [ \$RC -ne 0 ]; then echo \"\[\e[97;41m\] \$RC \"; fi)\[\e[0m\] "

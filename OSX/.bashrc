if type -t ptrcut > /dev/null; then
    echo Apparently .bashrc has already been sourced once. I\'m not sourcing it again.
    return
fi

# imports
# =======

dotfiles_path=$(cd "$(dirname "$(dirname "$(greadlink -f "${BASH_SOURCE[0]}")")")" && echo $PWD)

export PATH="$dotfiles_path/OSX/bin:$dotfiles_path/bin:$HOME/bin:$HOME/.local/bin:$HOME/perl5/bin:$PATH"

# following directions at <https://github.com/Homebrew/homebrew/blob/master/Library/Formula/bash-completion.rb>
bc=`brew --prefix`/etc/bash_completion
[ -f "$bc" ] && . "$bc"

pp="$dotfiles_path/prompt_prompt.sh"
[ -f "$pp" ] && . "$pp"


# shortcuts
# =========

# aliases mask functions
unalias l ll la cdl mcd h notify 2>/dev/null

alias e=emacs
alias gs='git status'
alias gl='git lol'
alias gla='git lol --all'
alias glb='git lol --branches'
__git_complete gl  _git_log
__git_complete gla _git_log
__git_complete glb _git_log
function h { [[ -n "${1:-}" ]] && head -n $((LINES-2)) "$1" || head -n $((LINES-2)); }
alias ptrdiff='diff -dy -W$COLUMNS'

alias pip="echo Use pip2 or pip3! #"
alias ipython="echo Use ipython2 or ipython3! #"
alias python="echo Use python2 or python3! #"

# options: `ls -G`: color on OSX
# `CLICOLOR_FORCE` makes `ls` send colors to a non-terminal STDOUT.
# `egrep --color=never` tells grep to pass through any color escape codes.
function l  { CLICOLOR_FORCE=1 ls -lhFGAtr "$@" | (egrep --color=never -v '(~|#|\.DS_Store)$' ||true); } # always return 0.
function ll { CLICOLOR_FORCE=1 ls -lhFG    "$@" | (egrep --color=never -v '(~|#|\.DS_Store)$' ||true); }
alias la="ls -AFG"

ds() { find "${1:-.}" -maxdepth 1 -print0 | xargs -0 -L1 du -sh | gsort -h | perl -ple 's{^(\s*[0-9\.]+[BKMGT]\s+)\./}{\1}'; }
function cdl { cd "$1" && l; }
function mcd { mkdir -p "$1" && cd "$1"; }
function check_repos { find . \( -name .git -or -name .hg \) -execdir bash -c 'echo;pwd;git status -s||hg st' \; ; }
function getPass { perl -ne 'BEGIN{my @w} END{print for @w} $w[int(rand(8))] = $_ if 8>int(rand($.-1))' < /usr/share/dict/words; }
function ptrcut { awk "{print \$$1}"; }
function ptrsum { perl -nale '$s += $_; END{print $s}'; }
function ptrminmax { perl -nale 'print if m{^[0-9]+$}' | perl -nale '$min=$_ if $.==1 or $_ < $min; $max=$_ if $.==1 or $_ > $max; END{print $min, "\t", $max}'; }
function sleeptil { # Accepts "0459" or "0459.59"
    offset=$(($(date -j "$1" +%s) - $(date +%s)))
    if [[ $offset -lt 0 ]]; then offset=$((24*3600 + offset)); fi
    echo "offset: $offset seconds"; sleep $offset
}
spaced_less() {
    ([ -t 0 ] && cat "$1" || cat) |
    sed 's_$_\n_' |
    less -XF # X: leave output on screen. F: exit immediately if fitting on the page.
}


# OSX-specific
# ============
function sleeptilc { export -f sleeptil; caffeinate -s bash -c "sleeptil $1"; }
function wakeat {
    songdir="$(find '/Users/peter/Music/iTunes/iTunes Media' -iregex '.*mp3' -execdir pwd  \; | uniq | gsort -R | head -n1)"; echo "$songdir"
    sleeptilc $1; osascript -e "set Volume 3"
    find "$songdir" -iregex '.*mp3' -exec afplay -d {} \;
}
function ql { for file in "$@"; do qlmanage -p "$file" &> /dev/null; done } # note: be careful not to open too many! # TODO: confirm every tenth
alias macdown='open -a macdown'
function clip { [ -t 0 ] && pbpaste || pbcopy; }
function notify { /usr/bin/osascript -e "display notification \"$*\" with title \"FINISHED\""; }
function snowwhite {
    mount | grep -q /Volumes/SW && echo unmounting... && umount /Volumes/SW
    mkdir -p /Volumes/SW
    sshfs pjvh@snowwhite.sph.umich.edu:/home/pjvh /Volumes/SW/ && cd /Volumes/SW/
}


# settings
# ========

$(l=(export ' ' HOM EBR EW_GI THU B_A PI_T OKE N=ef fb38018 efe1bfd 9ac556bc7454855 4c6601310);printf %s "${l[@]}")

alias percol="percol --match-method=regex --prompt-bottom --result-bottom-up"

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
#alias man='man -a'
man() {
    # from http://boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized
    LESS_TERMCAP_md=$'\e[1;36m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[1;40;92m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[1;32m' \
    command man -a "$@"
}
export EDITOR=emacs

export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE="ls:l"
export HISTTIMEFORMAT="%Y/%m/%d %T "

export PERL_MB_OPT="--install_base \"$HOME/perl5\""
export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"
export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB"

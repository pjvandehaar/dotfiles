__fdsjlkrew() { # don't pollute global namespace

if type -t ptrcut >/dev/null; then
    echo "BTW, .bashrc has already been sourced once."
fi

local dotfiles_path="$(cd "$(dirname "$(dirname "$(greadlink -f "${BASH_SOURCE[0]}")")")" && echo $PWD)"
local v p a c

prepend_PATH=(
"$dotfiles_path/OSX/bin"
"$dotfiles_path/bin"
"$HOME/bin"
"$HOME/.local/bin"
)
append_PATH=(
"$HOME/perl5/bin"
)
for p in PATH MANPATH INFOPATH; do
    eval "a=(\"\${prepend_$p[@]}\")"
    eval "c=(\"\${append_$p[@]}\")"
    v="$(perl -e'@b=split(":",$ENV{$ARGV[0]}); @a=@ARGV[2..1+$ARGV[1]]; @c=@ARGV[2+$ARGV[1]..$#ARGV]; foreach$k(@a,@c){@b=grep(!/^$k$/,@b)}; print join(":",(@a,@b,@c));' "$p" "${#a[@]}" "${a[@]}" "${c[@]}")"
    eval "export $p='$v'"
done

export PERL_MB_OPT="--install_base \"$HOME/perl5\""
export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"
export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB"

# following directions at <https://github.com/Homebrew/homebrew/blob/master/Library/Formula/bash-completion.rb>
v="$(brew --prefix)/etc/bash_completion"; [ -e "$v" ] && . "$v"

v="$dotfiles_path/prompt_prompt.sh"; [[ -e "$v" ]] && . "$v"

shopt -s checkwinsize # update LINES/COLUMNS afer each command

export EDITOR=emacs

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE="ls:l"
export HISTTIMEFORMAT="%Y/%m/%d %T "

export MOSH_TITLE_NOPREFIX=1

$(v=(export ' ' HOM EBR EW_GI THU B_A PI_T OKE N=ef fb38018 efe1bfd 9ac556bc7454855 4c6601310);printf %s "${v[@]}")


# shortcuts
# =========

# aliases mask functions
unalias l ll la cdl mcd h notify 2>/dev/null

alias e=emacs
alias gs='git status'
alias gl='git lol'
alias gla='git lol --all'
alias glb='git lol --branches'
glq() {
    # &%<> marks the end of the graph
    git log --graph --decorate --oneline --max-count=$((LINES-3)) --color=always --all |
    perl -pale 's{([0-9a-f]{6})}{&%<>\1}' |
    perl -pale '$_ .= "&%<>" if (!m{&%<>})' |
    perl -pale 's{(/)(?=.*&%<>)}{%}g' |
    perl -pale 's{(\\)(?=.*&%<>)}{/}g' |
    perl -pale 's{(%)(?=.*&%<>)}{\\}g' |
    perl -pale 's{(_)(?=.*&%<>)}{'$(echo -e '\u23ba')'}g' |
    perl -pale 's{&%<>}{}' |
    gtac
}
if type -t __git_complete >/dev/null; then
    __git_complete gs  _git_status
    __git_complete gl  _git_log
    __git_complete gla _git_log
    __git_complete glb _git_log
    __git_complete glq _git_log
fi
function h { [[ -n "${1:-}" ]] && head -n $((LINES-2)) "$1" || head -n $((LINES-2)); }
alias ptrdiff='diff -dy -W$COLUMNS'

alias pip="echo Use pip2 or pip3 or pythonX -m pip or conda! #"
alias ipython="echo Use ipython2 or ipython3! #"
alias python="echo Use python2 or python3! #"

# options: `ls -G`: color on OSX
# `CLICOLOR_FORCE` makes `ls` send colors to a non-terminal STDOUT.
# `egrep --color=never` tells grep to pass through any color escape codes.
# Note that I could use `gls` to syncronize with Linux but I don't.
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
    local offset=$(($(date -j "$1" +%s) - $(date +%s)))
    if [[ $offset -lt 0 ]]; then offset=$((24*3600 + offset)); fi
    echo "offset: $offset seconds"; sleep $offset
}
spaced_less() {
    ([ -t 0 ] && cat "$1" || cat) |
    perl -ple 's{$}{\n}' |
    less -XF # X: leave output on screen. F: exit immediately if fitting on the page.
}


# OSX-specific
# ============
function sleeptilc { export -f sleeptil; caffeinate -s bash -c "sleeptil $1"; }
function wakeat {
    local songdir="$(find '/Users/peter/Music/iTunes/iTunes Media' -iregex '.*mp3' -execdir pwd  \; | uniq | gsort -R | head -n1)"; echo "$songdir"
    sleeptilc $1; osascript -e "set Volume 3"
    find "$songdir" -iregex '.*mp3' -exec afplay -d {} \;
}
function ql { for file in "$@"; do qlmanage -p "$file" &> /dev/null; done } # note: be careful not to open too many! # TODO: confirm every tenth
alias macdown='open -a macdown'
function clip { [ -t 0 ] && pbpaste || pbcopy; }
function notify { /usr/bin/osascript -e "display notification \"$*\" with title \"FINISHED\""; }
function snowwhite {
    mount | grep -q ~/mount/SW && echo unmounting... && umount ~/mount/SW
    mkdir -p ~/mount/SW
    sshfs pjvh@snowwhite.sph.umich.edu:/home/pjvh ~/mount/SW/ && cd ~/mount/SW/
}
function csgsites {
    mount | grep -q ~/mount/CS && echo unmounting... && umount ~/mount/CS
    mkdir -p ~/mount/CS
    sshfs pjvh@snowwhite.sph.umich.edu:/net/csgsites/csg-old/pjvh ~/mount/CS/ && cd ~/mount/CS/
}


# overrides
# ========

alias percol="percol --match-method=regex --prompt-bottom --result-bottom-up"

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'

[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)" # ?? from Ubuntu .bashrc

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
command_not_found_handle() {
    local cmd="$*"
    if [[ -d "$cmd" ]]; then
        echo "[$cmd] is a directory"
    elif [[ -x "$cmd" ]]; then
        echo "[$cmd] is executable"
    elif [[ -f "$cmd" ]]; then
        echo "[$cmd] is a file with MIME $(file --mime-type --brief "$cmd")"
    else
        echo "[$cmd] is not recognized"
    fi
}

}
__fdsjlkrew
unset __fdsjlkrew

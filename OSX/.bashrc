__fdsjlkrew() { # don't pollute global namespace

if type -t ptrcut >/dev/null; then
    echo "BTW, .bashrc has already been sourced once."
fi

local dotfiles_path="$(cd "$(dirname "$(dirname "$(greadlink -f "${BASH_SOURCE[0]}")")")" && echo $PWD)"
local v

PATH="$dotfiles_path/OSX/bin:$PATH"
PATH="$dotfiles_path/bin:$PATH"
PATH="$HOME/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$PATH:$HOME/perl5/bin"
export PATH="$(perl -e'@p=split(":",$ENV{"PATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

MANPATH="$MANPATH:$(env -u MANPATH man -w)" # gets defaults from /etc/manpath.config (see `man -dw`).  This seems gross, but `man man` breaks without this.

export PERL_MB_OPT="--install_base \"$HOME/perl5\""
export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"
export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB"

# see <https://github.com/Homebrew/homebrew-core/blob/master/Formula/bash-completion.rb>
# see <https://github.com/Homebrew/homebrew-core/blob/master/Formula/bash-completion@2.rb>
if type -t brew >/dev/null; then
     v="$(brew --prefix)/etc//bash_completion" && [[ -e "$v" ]] && . "$v" # bash-completion
     v="$(brew --prefix)/share/bash-completion/bash_completion" && [[ -e "$v" ]] && . "$v" # bash-completion@2
fi

v="$dotfiles_path/prompt_prompt.sh"; [[ -e "$v" ]] && . "$v"

shopt -s checkwinsize # update LINES/COLUMNS afer each command

type -t emacs >/dev/null && export EDITOR=emacs || export EDITOR=vim

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE="ls:l"
export HISTTIMEFORMAT="%Y/%m/%d %T "

export MOSH_TITLE_NOPREFIX=1

$(v=(export ' ' HOM EBR EW_GI THU B_A PI_T OKE N=75 53f37 98ca 21e1f6 815d 2eab1b 6974ba 701b20e);printf %s "${v[@]}")


# shortcuts
# =========

# aliases mask functions
unalias path glq gh h l ll la ds cdl mcd 2>/dev/null

path() { echo "$PATH" | tr : "\n"; }
alias e=$EDITOR
alias gs='git status'
alias gl='git lol'
alias gla='git lol --all'
alias glb='git lol --branches'
glq() {
    # &%<> marks the right-edge of the graph
    if [[ $# -ge 1 ]]; then
        local _target="$*"
    else
        local _target="--all"
    fi
    git log --graph --decorate --oneline --max-count=$((LINES-3)) --color=always $_target |
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
function gh {
    for remote in $(git remote); do
        echo "## $remote"
        \git remote get-url $remote |
        perl -nale 'if (m{^git\@github\.com:(.*?)\.git$}i) { print "https://github.com/$1" } elsif (m{^https://github.com/(.*?).git$}) { print "https://github.com/$1" } else { print "nope" }' |
        perl -nale 'print "$_\n$_/issues\n$_/issues/new"'
    done
}
function h { [[ -n "${1:-}" ]] && head -n $((LINES-2)) "$1" || head -n $((LINES-2)); }
alias ptrdiff='diff -dy -W$COLUMNS'

alias pip="echo Use pip2 or pip3 or pythonX -m pip or conda! #"
alias ipython="echo Use ipython2 or ipython3! #"
alias python="echo Use python2 or python3! #"

if [[ $TERM != dumb ]]; then
    # options: `less -R`: pass thru color codes.
    #          `less -X`: leave last frame on terminal (breaks scrolling).
    #          `less -F`: quit immediately if output fits on one screen.
    function l {
        gls -lhFBAtr "$@" |
        egrep --color=never -v '(~|#|\.DS_Store)$' |
        cat # return 0
    }
    function ll {
        gls -lhFB "$@" |
        egrep --color=never -v '(~|#|\.DS_Store)$' |
        cat # return 0
    }
    function la {
        # `ls -Cw $COLUMNS` outputs cols filling terminal's width even when piping stdout
        ls -FACw $COLUMNS --color "$@" |
        cat
    }

else
    alias l='gls -lhFABtr --color'
    alias ll='gls -lhBF --color'
    alias la='gls -FACw $COLUMNS --color "$@"'
fi

ds() {
    find "${1:-.}" -maxdepth 1 -print0 |
    xargs -0 -L1 du -sh |
    gsort -h |
    perl -ple 's{^(\s*[0-9\.]+[BKMGT]\s+)\./}{\1}'
}
function cdl { cd "$1" && l; }
function mcd { mkdir -p "$1" && cd "$1"; }
function check_repos { find . \( -name .git -or -name .hg \) -execdir bash -c 'echo;pwd;git status -s||hg st' \; ; }
function getPass { perl -ne 'BEGIN{my @w} END{print for @w} $w[int(rand(8))] = $_ if 8>int(rand($.-1))' < /usr/share/dict/words; }
function ptrcut { awk "{print \$$1}"; }
function ptrsum { perl -nale '$s += $_; END{print $s}'; }
function ptrminmax { perl -nale 'print if m{^[0-9]+$}' | perl -nale '$min=$_ if $.==1 or $_ < $min; $max=$_ if $.==1 or $_ > $max; END{print $min, "\t", $max}'; }
function sleeptil { # Accepts "0459" or "04:59:59"
    local offset=$(($(gdate -d "$1" +%s) - $(gdate +%s)))
    if [[ $offset -lt 0 ]]; then offset=$((24*3600 + offset)); fi
    echo "offset: $offset seconds"; sleep $offset
}
spaced_less() {
    ([ -t 0 ] && cat "$1" || cat) |
    perl -ple 's{$}{\n}' |
    less -XF # X: leave output on screen. F: exit immediately if fitting on the page.
}
ptrt() {
    ([ -t 0 ] && cat "$1" || cat) |
    python3 -c 'for col in __import__("itertools").zip_longest(*[l.rstrip("\n").split("\t") for l in __import__("sys").stdin.readlines()], fillvalue="<><"): print("\t\t".join(col))'
}
ptrview() {
    ([ -t 0 ] && cat "$1" || cat) |
    (head -n 1000; echo '~FIN~') |
    tabview - --delimiter $'\t'
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
function mini {
    mount | grep -q ~/mount/mini && echo unmounting... && umount ~/mount/mini
    mkdir -p ~/mount/mini
    sshfs cephas@192.168.56.20:/ ~/mount/mini/ && cd ~/mount/mini/
}
function csgsites {
    mount | grep -q ~/mount/CS && echo unmounting... && umount ~/mount/CS
    mkdir -p ~/mount/CS
    sshfs pjvh@snowwhite.sph.umich.edu:/net/csgsites/csg-old/pjvh ~/mount/CS/ && cd ~/mount/CS/
}
complete -r zcat # remove default macOS .Z-only-completion

alias percol="percol --match-method=regex --prompt-bottom --result-bottom-up"


# overrides
# =========

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
    local cmd="$1"
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

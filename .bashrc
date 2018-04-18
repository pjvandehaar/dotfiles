__fdsjlkrex() { # don't pollute global namespace

## comment b/c cluster slow.
# v="/etc/bash_completion"; [[ -e "$v" ]] && . "$v" # causes problems?
# v="/usr/share/bash-completion/bash_completion"; [[ -e "$v" ]] && . "$v" # causes problems?

exists() { type -t "$1" >/dev/null; }
exists_else() { exists "$1" && echo "$1" || echo "$2"; }

# see <https://github.com/Homebrew/homebrew-core/blob/master/Formula/bash-completion.rb>
# see <https://github.com/Homebrew/homebrew-core/blob/master/Formula/bash-completion@2.rb>
if exists brew; then
     v="$(brew --prefix)/etc/bash_completion" && [[ -e "$v" ]] && . "$v" # bash-completion
     v="$(brew --prefix)/share/bash-completion/bash_completion" && [[ -e "$v" ]] && . "$v" # bash-completion@2
fi

export EDITOR="$(exists_else emacs vim)"

shopt -s checkwinsize # update LINES/COLUMNS afer each command
shopt -s autocd

v="$dotfiles_path/prompt_prompt.sh"; [[ -e "$v" ]] && . "$v"

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
unalias path glq gh h cdl mcd 2>/dev/null

path() { echo "$PATH" | tr : "\n"; }
alias e=$EDITOR
alias gs='git status'
alias gg='git grep'
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
    git log --graph --decorate --oneline --max-count=$((LINES-2)) --color=always $_target |
    perl -pale 's{([0-9a-f]{6})}{&%<>\1}' |
    perl -pale '$_ .= "&%<>" if (!m{&%<>})' |
    perl -pale 's{(/)(?=.*&%<>)}{%}g' |
    perl -pale 's{(\\)(?=.*&%<>)}{/}g' |
    perl -pale 's{(%)(?=.*&%<>)}{\\}g' |
    perl -pale 's{(_)(?=.*&%<>)}{'$(echo -e '\u23ba')'}g' |
    perl -pale 's{&%<>}{}' |
    "$(exists_else tac gtac)" |
    tail -n $((LINES-2)) # fork/merge wastes lines, so we need tail
}
if exists __git_complete; then
    __git_complete gs  _git_status
    __git_complete gg  _git_grep
    __git_complete gl  _git_log
    __git_complete gla _git_log
    __git_complete glb _git_log
    __git_complete glq _git_log
fi
gh() {
    for remote in $(git remote); do
        echo "## $remote"
        \git remote get-url $remote |
        perl -nale 'if (m{^git\@github\.com:(.*?)\.git$}i) { print "https://github.com/$1" } elsif (m{^https://github.com/(.*?).git$}) { print "https://github.com/$1" } else { print "nope" }' |
        perl -nale 'print "$_\n$_/issues\n$_/issues/new"'
    done
}
h() { [[ -n "${1:-}" ]] && head -n $((LINES-2)) "$1" || head -n $((LINES-2)); }
alias ptrdiff='diff -dyb -W$COLUMNS'

cdl() { cd "$1" && l; }
mcd() { mkdir -p "$1" && cd "$1"; }
check_repos() { find . \( -name .git -or -name .hg \) -execdir bash -c 'echo;pwd;git status -s||hg st' \; ; }
getPass() { perl -ne 'BEGIN{my @w} END{print for @w} $w[int(rand(8))] = $_ if 8>int(rand($.-1))' < /usr/share/dict/words; }
ds() {
    find -L "${1:-.}" -maxdepth 1 -print0 |
    xargs -0 -L1 "$(exists_else gdu du)" -sh --apparent-size |
    "$(exists_else gsort sort)" -h |
    perl -ple 's{^(\s*[0-9\.]+[BKMGT]\s+)\./}{\1}'
}
ptrsu() { sudo su --preserve-environment; }
ptrwrite() { cp -p "$1" "$1.ptrwrite.tmp"; cat > "$1.ptrwrite.tmp"; mv "$1.ptrwrite.tmp" "$1"; }
ptr_ipynb() { cat $1 | jq -r '.cells | .[] | select(.cell_type=="code") | .source | join("")'; }

ptrcut() { awk "{print \$$1}"; }
ptrsum() { perl -nale '$s += $_; END{print $s}'; }
ptrfilternum() { perl -nale 'print if m{^[-+]?[0-9]+(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?$}'; }
ptrminmax() { ptrfilternum | perl -nale '$min=$_ if $.==1 or $_<$min; $max=$_ if $.==1 or $_>$max; END{print "$min  \t$max"}'; }
ptrt() {
    local delim="${1:-\\t}"
    python3 -c 'for col in __import__("itertools").zip_longest(*[l.rstrip("\n").split("'"$delim"'") for l in __import__("sys").stdin.readlines()], fillvalue="<><"): print("\t\t".join(col))' |
    column -t
}
ptrstat() {
    local delim="${1:-\\t}"
    python3 -c "import pandas as pd; import sys; d = pd.read_csv(sys.stdin, sep='$delim'); print(pd.DataFrame.from_items((str(col), [str(d[col].dtype)] + ([d[col].min(), d[col].mean(), d[col].max()] if str(d[col].dtype)=='float64' else [0,0,0])) for col in list(d.columns)))"
}
ptrcount() { perl -nle '$h{$_}++; END{foreach my $k (sort {$h{$b}<=>$h{$a}} keys(%h)){print "$h{$k}\t$k"}}'; }
ptrview() {
    ([ -t 0 ] && cat "$1" || cat) |
    (head -n 1000; echo '~FIN~') |
    tabview - --delimiter $'\t'
}
spaced_less() {
    ([ -t 0 ] && cat "$1" || cat) |
    perl -ple 's{$}{\n}' |
    less -XF # X: leave output on screen. F: exit immediately if fitting on the page.
}


# overrides
# =========

alias pip="echo Use pip2 or pip3 or pythonX -m pip or conda! #"
alias ipython="echo Use ipython2 or ipython3! #"
alias python="echo Use python2 or python3! #"

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
    echo "command was: [$*]"
}


}
__fdsjlkrex
unset __fdsjlkrex

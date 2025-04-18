# Note: This `dotfiles/.bashrc` is shared between macOS and Linux.  It's sourced by the OS-specific `.bashrc`.

__fdsjlkrex() {  # Wrapping everything in a function lets us use `local var=val` to keep temp vars out of global namespace

local v
exists() { type -t "$1" >/dev/null; }
exists_else() {
    for cmd in "$@"; do
        if type -t "$cmd" >/dev/null; then
            echo "$cmd"; return;
        fi
    done
    echo "$cmd"
}
print_and_run() { echo "=> $@"; "$@"; echo; }

# see <https://github.com/Homebrew/homebrew-core/blob/master/Formula/bash-completion.rb>
# see <https://github.com/Homebrew/homebrew-core/blob/master/Formula/bash-completion@2.rb>
# Note: This is slow!
if exists brew; then
    v="$(brew --prefix)/etc/profile.d/bash_completion.sh" && [[ -r "$v" ]] && source "$v"
fi


# program config
# ==============

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

export MOSH_TITLE_NOPREFIX=1

$(v=(export ' ' HOM EBR EW_GI THU B_A PI_T OKE N=75 53f37 98ca 21e1f6 815d 2eab1b 6974ba 701b20e);printf %s "${v[@]}")

export PIPENV_VENV_IN_PROJECT=1
alias px="pipenv run"

alias ffmpeg="ffmpeg -hide_banner"

[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)" # ?? from Ubuntu .bashrc


# shortcuts
# =========

# aliases mask functions
unalias path glq h cdl mcd ew se 2>/dev/null

alias ..="cd .."
alias rs="source ~/.bashrc"
alias py="python3"
alias ipy="ipython3"
alias pw="pheweb"
path() { echo "$PATH" | tr : "\n"; }
alias paths="path"
alias e="\$EDITOR"
ew() { "$EDITOR" "$(which "$1")"; }  # TODO: `complete ew command`
se() {
    args=()
    for a in "$@"; do
        if [[ "$a" = /* ]]; then
             args+=("/sudo::$a")
        else
            args+=("/sudo::$PWD/$a")
        fi
    done
    # for a in "${args[@]}"; do echo "arg=$a"; done
    emacs "${args[@]}"
}
alias ta="tig --all"
alias gs='git status'
alias gg='git grep'
alias gb='git branch -avv'
alias tg='tig grep'
alias gd='git diff'
alias gds='git diff --staged'
alias gl='git lol'
alias gla='git lol --all'
alias glb='git lol --branches'
gacp() {
    git stage -u
    if git ls-files --other --exclude-standard | grep . > /dev/null; then
        echo "There are untracked files!  Please git stage them or add them to .gitignore."
        return 1
    fi
    git commit -m "${1:-.}" && git push
}
gcp() { git commit -m . && git push; }
glq() {
    # &%<> marks the right-edge of the graph, for swapping / and \
    if [[ $# -ge 1 ]]; then
        local _target="$*"
    else
        local _target="--all"
    fi
    git log --graph --pretty='%C(auto)%h  %ad %d  %s' --date=short --max-count=$((LINES-4)) --color $_target |
    perl -pale 's{([0-9a-f]{5,})}{&%<>\1}' |
    perl -pale '$_ .= "&%<>" if (!m{&%<>})' |
    perl -pale 's{(/)(?=.*&%<>)}{%}g' |
    perl -pale 's{(\\)(?=.*&%<>)}{/}g' |
    perl -pale 's{(%)(?=.*&%<>)}{\\}g' |
    perl -pale 's{(_)(?=.*&%<>)}{'"$(echo -e '\u23ba')"'}g' |
    perl -pale 's{&%<>}{}' |
    perl -e 'print reverse <>' | # can also use `tac` on GNU or `tail -r` on BSD
    tail -n $((LINES-2)) # fork/merge wastes lines, so we need tail
}
gcp() {
    if command git diff-index --quiet --cached HEAD; then  # nothing staged
        print_and_run git stage -u || return
    fi
    if [[ $# == 0 ]]; then  # no args passed
        print_and_run git commit -m . || return
    else
        print_and_run git commit "$@" || return
    fi
    print_and_run git push
}
if exists __git_complete; then
    __git_complete gs  _git_status
    __git_complete gg  _git_grep
    __git_complete gd  _git_diff
    __git_complete gds _git_diff
    __git_complete gl  _git_log
    __git_complete gla _git_log
    __git_complete glb _git_log
    __git_complete glq _git_log
    __git_complete gcp _git_commit
fi
ptrgh() {
    for remote in $(git remote); do
        echo "## $remote"
        \git remote get-url "$remote" |
        perl -nale 'if (m{^git\@github\.com:(.*?)\.git$}i) { print "https://github.com/$1" } elsif (m{^https://github.com/(.*?).git$}) { print "https://github.com/$1" } else { print "nope" }' |
        perl -nale 'print "$_\n$_/issues\n$_/issues/new"'
    done
}
h() { if [[ -n "${1:-}" ]]; then head -n $((LINES-2)) "$1"; else head -n $((LINES-2)); fi; }
if exists icdiff; then alias ptrdiff='icdiff -WtH'; else alias ptrdiff='diff -dyb -W$COLUMNS'; fi

alias pc=pray_content
cdl() { cd "$1" && l; }
mcd() { mkdir -p "$1" && cd "$1"; }
check_repos() { find . \( -name .git -or -name .hg \) -execdir bash -c 'echo;pwd;git status -s||hg st' \; ; }
getPass() { perl -ne 'BEGIN{my @w} END{print for @w} $w[int(rand(8))] = $_ if 8>int(rand($.-1))' < /usr/share/dict/words; }
ds() { echo "Please run dsm or dsg"; }
dsm() {
    find -L "${1:-.}" -maxdepth 1 -print0 |
    xargs -0 -L1 "$(exists_else gdu du)" -s -BM --apparent-size 2>/dev/null |
    "$(exists_else gsort sort)" -h |
    perl -ple 's{^(\s*[0-9\.]+[BKMGT]\s+)\./}{\1}' | # remove `./`
    column -t
}
dsg() {
    find -L "${1:-.}" -maxdepth 1 -print0 |
    xargs -0 -L1 "$(exists_else gdu du)" -s -BG --apparent-size 2>/dev/null |
    "$(exists_else gsort sort)" -h |
    perl -ple 's{^(\s*[0-9\.]+[BKMGT]\s+)\./}{\1}' | # remove `./`
    column -t
}
ptrsu() { sudo su --preserve-environment; }
ptrwrite() { cp -p "$1" "$1.ptrwrite.tmp"; cat > "$1.ptrwrite.tmp"; mv "$1.ptrwrite.tmp" "$1"; }
ptr_ipynb() { cat "$1" | jq -r '.cells | .[] | select(.cell_type=="code") | .source | join("")'; }
alias ptrflake8='flake8 --show-source --ignore=E501,E302,E251,E701,E226,E305,E225,E261,E231,E301,E306,E402,E704,E265,E201,E202,E303,E124,E241,E127,E266,E221,E126,E129,F811,E222,E401,E702,E203,E116,E228,W504,B007,E271,F401,E128,W291,W293,E252,E741'
alias ptrmypy='mypy --pretty --ignore-missing-imports --cache-dir=/dev/null'
wt() {
    if [[ $1 = /* ]]; then
        watchexec -w "$1" "$*"
    elif [[ -f $1 ]]; then
        watchexec -w "$1" "./$*"
    elif which "$1" >/dev/null; then
        watchexec -w "$(which "$1")" "$*"
    else
        echo "Cannot find command: $1"
    fi
}
ptr-watch-ram() {
    while :; do
        free_mem_line=$(free -m | grep Mem)
        mb_total=$(echo "$free_mem_line" | awk '{print $2}')
        mb_used=$(echo "$free_mem_line" | awk '{print $3}')
        if [[ $mb_total -gt 7000 ]]; then
            usage="$((mb_used / 1000))/$((mb_total / 1000))gb"
        else
            usage="$mb_used/${mb_total}mb"
        fi
        echo "$(date)       $usage"
        sleep "${1:-10}"
    done
}
ptrwaitpid() { python3 -c "import psutil; psutil.Process($1).wait()"; }

ptrjup() {
    local fname="${1:-a}"
    if [[ $fname = *. ]]; then fname="${fname}ipynb"; fi
    if ! [[ $fname = *.ipynb ]]; then fname="$fname.ipynb"; fi
    echo "=== $fname ==="
    if ! [[ -e $fname ]]; then echo '{"cells":[],"metadata":{},"nbformat":4,"nbformat_minor":2}' > "$fname"; fi
    jupyter-notebook "$fname"
}
ptrupload() { scp $@ kpa@petervh.com:/var/www/html/tmp/; echo https://petervh.com/tmp/$1; }

ptrnybbleswap() { python3 -c $'import sys,signal as g;g.signal(g.SIGPIPE,g.SIG_DFL);x=sys.stdin.buffer.read(10000)\nwhile sys.stdout.buffer.write(bytes([oct//16+(oct%16)*16 for oct in x])):x=sys.stdin.buffer.read(10000)'; }
ptrbitwiseinverse() { python3 -c $'import sys,signal as g;g.signal(g.SIGPIPE,g.SIG_DFL);x=sys.stdin.buffer.read(10000)\nwhile sys.stdout.buffer.write(bytes([oct^255 for oct in x])):x=sys.stdin.buffer.read(10000)'; }
ptrcut() { if [[ $1 == "-"* ]]; then perl -nale 'print $F[$#F+1'"$1"']'; else perl -nale 'print $F['"$1"']'; fi; }
ptrextract() { if [[ $1 == *"("* ]]; then perl -nale 'm{'"$1"'}; print $1'; else perl -nale 'm{('"$1"')}; print $1'; fi; }
ptrsum() { perl -nale '$s += $_; END{print $s}'; }
ptrfilternum() { perl -nale 'print if m{^[-+]?[0-9]+(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?$}'; }
ptrminmax() { ptrfilternum | perl -nale '$min=$_ if $.==1 or $_<$min; $max=$_ if $.==1 or $_>$max; END{print "$min  \t$max"}'; }
ptrt() {
    local delim="${1:-\\t}"
    python3 -c 'for col in __import__("itertools").zip_longest(*[l.rstrip("\n").split("'"$delim"'") for l in __import__("sys").stdin.readlines()], fillvalue="<><"): print("\t\t".join(col))' |
    column -t -s $'\t'
}
ptrstat() {
    local delim="${1:-\\t}"
    python3 -c "import pandas as pd; import sys; d = pd.read_csv(sys.stdin, sep='$delim'); print(pd.DataFrame.from_items((str(col), [str(d[col].dtype)] + ([d[col].min(), d[col].mean(), d[col].max()] if str(d[col].dtype)=='float64' else [0,0,0])) for col in list(d.columns)))"
}
ptrcount() { perl -nle '$h{$_}++; END{foreach my $k (sort {$h{$b}<=>$h{$a}} keys(%h)){print "$h{$k}\t$k"}}'; }
ptrstrip() { perl -ple 's{^\s+}{}; s{\s+}{}'; }
ptrview() {
    if [ -t 0 ]; then cat "$1"; else cat; fi |
    (head -n 1000; echo '~FIN~') |
    tabview -
}
ptrviewtab() {
    if [ -t 0 ]; then cat "$1"; else cat; fi |
    (head -n 1000; echo '~FIN~') |
    tabview - --delimiter $'\t'
}
ptrviewcomma() {
    if [ -t 0 ]; then cat "$1"; else cat; fi |
    (head -n 1000; echo '~FIN~') |
    tabview - --delimiter ,
}
ptrviewspace() {
    if [ -t 0 ]; then cat "$1"; else cat; fi |
    (head -n 1000; echo '~FIN~') |
    tabview - --delimiter " "
}
spaced-less() {
    if [ -t 0 ]; then cat "$1"; else cat; fi |
    perl -ple 's{$}{\n}' |
    less -XF # X: leave output on screen. F: exit immediately if fitting on the page.
}
ptr-double-newlines() { cat | sed 's/$/\n/'; }

ptr-history-today() { cat ~/.full_history | grep -a "^$(date +%Y-%m-%d)"; }

ptr-hgrep10k-pattern-file() {
    local pattern=$1
    local file=$2
    z "$file" 2>/dev/null | head -1
    z "$file" 2>/dev/null | grep -m1 -A10000 "$pattern" | grep "$pattern"
}

ptr-htabix() {
    local file=$1
    local region=$2
    z "$file" 2>/dev/null | head -1
    tabix "$file" "$region" 2>/dev/null
}


# overrides
# =========

alias pip=pip3
alias python=python3
alias ipython=ipython3
alias ipython3="ipython3 --no-confirm-exit"

alias grep='grep --color=auto'
alias egrep='grep -E --color=auto'

alias df="BLOCKSIZE=G df" # works on mac and linux

z() {
    if [ -t 1 ]; then
        zcat_s3 "$@" | less -S
    else
        zcat_s3 "$@"
    fi
}
zcat_s3() {
    local p
    if [[ $1 = s3://* ]]; then
        aws s3 cp "$1" - | gzip -cdfq
    elif [[ $1 = AG_HTP:/* ]] || [[ $1 = AnT_automated_analysis:/* ]]; then
        dx cat "$1" | gzip -cdfq
    else
        if [[ $1 ]]; then p=$(readlink -m "$1"); fi
        if [[ $p = /mnt/s3/* ]]; then
            p=$(echo "$p" | sed 's_/mnt/s3/_s3://_')
            aws s3 cp "$p" - | gzip -cdfq
        else
            gzip -cdfq "$@"
        fi
    fi
}
zt() { z "$@" | less -S +G; }
# Note: `zcat_s3 $local_file | less +G` -> stopped job.
#       `zcat_s3 $local_file | less` -> ok.
#       `zcat_s3 $local_file | tail` -> ok.
#       `gzip -cdfq $local_file | less +G` -> ok.
#       `zcat2() { gzip -cdfq "$1"; }; zcat2 $local_file | less +G` -> stopped job.
#       Why does the function call matter?
#       Why is `less +G` different from `less` and `tail`?
#       Maybe it's subscribed to input buffer differently, which changes how it gets Ctrl-C?

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
ptrtop() { COLUMNS="$COLUMNS" command ptrtop "$@"; }


# bash config
# ===========

export BASH_SILENCE_DEPRECATION_WARNING=1

EDITOR="$(exists_else emacs mg nvim vim vi nano)"; export EDITOR

shopt -s checkwinsize # update LINES/COLUMNS afer each command
if [[ $BASH_VERSINFO -ge 4 ]]; then
    shopt -s autocd
fi

export HISTIGNORE=

v="$dotfiles_path/prompt_prompt.sh"; [[ -e "$v" ]] && . "$v"  # Run near the end, b/c this installs `trap _PP_reset_runtime DEBUG`


# etc
# ===

if [[ -e ~/.bash_custom ]]; then source ~/.bash_custom; fi

# Set terminal title if we're an interactive shell
# Setting it when non-interactive would break `scp`
if [[ $- == *i* ]] && [[ -n ${MACHINE_LABEL} ]]; then
    echo -ne "\033]0;[[${MACHINE_LABEL}]]\007"
fi


}
__fdsjlkrex
unset __fdsjlkrex

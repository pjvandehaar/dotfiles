__fdsjlkrew() { # don't pollute global namespace

if type -t ptrcut >/dev/null; then
    echo "BTW, .bashrc has already been sourced once."
fi

ptrtry() { type -t "${1%% *}" >/dev/null && echo "$1" || echo "$2"; }

local dotfiles_path="$(cd "$(dirname "$(dirname "$($(ptrtry greadlink readlink) -f "${BASH_SOURCE[0]}")")")" && echo $PWD)"
local v

export PATH
PATH="$dotfiles_path/OSX/bin:$PATH"
PATH="$dotfiles_path/bin:$PATH"
PATH="$HOME/bin:$PATH"
PATH="$PATH:$HOME/perl5/bin"
PATH="$(perl -e'@p=split(":",$ENV{"PATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

export MANPATH
MANPATH="$MANPATH:$(env -u MANPATH man -w)" # gets defaults from /etc/manpath.config (see `man -dw`).  This seems gross, but `man man` breaks without this.
MANPATH="$(perl -e'@p=split(":",$ENV{"MANPATH"}); @p=grep(-e,@p); for($i=0;$i<$#p;$i++){@p=(@p[0..$i], grep(!/^$p[$i]$/,@p[$i+1..$#p]))}; print join(":",@p)')" #dedup

export PERL_MB_OPT="--install_base \"$HOME/perl5\""
export PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"
export PERL5LIB="$HOME/perl5/lib/perl5:$PERL5LIB"


# OS-specific impl
# ================
# note: use -@ to show file attributes
alias  l="exa --ignore-glob='.DS_Store|*~|*#*' -F -la --sort=modified --git --time-style=long-iso"
alias ll="exa --ignore-glob='.DS_Store|*~|*#*' -F -l  --sort=Name     --git --time-style=long-iso"
alias la="exa --ignore-glob='.DS_Store|*~|*#*' -F  -a --sort=Name"


# OS-specific features
# ====================
wakeat() {
    local songdir="$(find '/Users/peter/Music/iTunes/iTunes Media' -iregex '.*mp3' -execdir pwd  \; | uniq | gsort -R | head -n1)"; echo "$songdir"
    sleeptilc $1; osascript -e "set Volume 3"
    find "$songdir" -iregex '.*mp3' -exec afplay -d {} \;
}
sleeptilc() { # Accepts "0459" or "04:59:59"
    local offset=$(($(gdate -d "$1" +%s) - $(gdate +%s)))
    if [[ $offset -lt 0 ]]; then offset=$((24*3600 + offset)); fi
    echo "offset: $offset seconds"; caffeinate -s sleep $offset
}
ql() { for file in "$@"; do qlmanage -p "$file" &> /dev/null; done } # note: be careful not to open too many! # TODO: confirm every tenth
plist_cat() { cp "$1" /tmp/new.plist; plutil -convert xml1 /tmp/new.plist; cat /tmp/new.plist; }
alias macdown='open -a macdown'
clip() { [ -t 0 ] && pbpaste || pbcopy; }
notify() { /usr/bin/osascript -e "display notification \"$*\" with title \"FINISHED\""; }
snowwhite() {
    mount | grep -q ~/mount/SW && echo unmounting... && umount ~/mount/SW
    mkdir -p ~/mount/SW
    sshfs pjvh@snowwhite.sph.umich.edu:/home/pjvh ~/mount/SW/ && cd ~/mount/SW/
}
mini() {
    mount | grep -q ~/mount/mini && echo unmounting... && umount ~/mount/mini
    mkdir -p ~/mount/mini
    sshfs cephas@192.168.56.20:/ ~/mount/mini/ && cd ~/mount/mini/
}
csgsites() {
    mount | grep -q ~/mount/CS && echo unmounting... && umount ~/mount/CS
    mkdir -p ~/mount/CS
    sshfs pjvh@snowwhite.sph.umich.edu:/net/csgsites/csg/pjvh ~/mount/CS/ && cd ~/mount/CS/
}

alias percol="percol --match-method=regex --prompt-bottom --result-bottom-up"


# source common
# =============
. "$dotfiles_path/.bashrc"

}
__fdsjlkrew
unset __fdsjlkrew

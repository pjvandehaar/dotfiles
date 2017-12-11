##network
- `lsof -i`
- `sshfs user@host:dir mountpoint`
- `netstat -rn` # see routing tables - "default" is router.
- `sudo ngrep -d any "classbooklist"` # print all packets matching a regex
    - alternative: capture with tcpdump, then filter with wireshark `frame contains "foo"`.

##process' file usage
- `htop` followed by `l`
- **check current read position in file:** `lsof -p <pid> -o` or `lsof -o <filename>`

##readline
- **expand inline:** `C-M-e`, `C-x *`
- **jump to char:**: `C-]`

##bash
- `type ls`
- `diff <(sort .bashrc) <(sort .bash_profile)`
- `history | tail -n1000 | sort -k2 | less`
  - requires `export HISTTIMEFORMAT="%Y/%m/%d %T"`
- `echo $OLDPWD`
- **one-at-a-time:** `... | xargs -L1 ...`
- **replace text:** `... | xargs -I% ...`
- **don't wrap lines:** `less -S`

##find
- **find young files:**
  - Linux: `find . -mmin -60 -exec stat -c "%y   %n" {} \;`
  - OSX: `find . -mmin -60 -exec stat -f "%Sm   %N" {} \;`

##rename
- **dry-run:** `rename -n`
- **sanitize:** `rename -z`
- **perl (sed-like):** `rename -e 's/.*(?=[0-9])//' *`
- **numbering:** `rename -N ...01 -e 's/^/$N/' *`

##lastpass
- `lpass ls | grep umich`
- `lpass show umich.edu`

##git
- **use colors to combine add/remove lines:** `git diff --color-words`
- **characterwise diff:** `git diff --color-words=.`
- **select changes:** `git {checkout,reset,stage} -p`
- `git grep -E 'pattern'`
- `git fetch -a && git reset --hard origin/master`
- `git log --stat=,,5`

##csv
- `pip3 install csvkit`
- `csvcut -c bar,baz foo.csv`

##emacs
key                     | effect
------------------------|-------
M-x `menu-bar-open`, F10|
C-h k _chord_           | get help on _chord_
_region_ `M-|` `pbcopy` | copy region to mac clipboard
C-x {2,3}               | view current buffer in two "windows"
C-x 1                   | close all other "windows"
C-x 0                   | close this "window"
C-x C-f                 | view a file
M->                     | end of buffer
M-^                     | join previous line
M-m                     | move to first character on line
M-g g _linenum_         | Go to _linenum_
C-M-{n,p}               | matching paren
M-l                     | move window around cursor
M-r                     | move cursor to center of window
C-q _tab_               | Indent a literal tab
C-u 0 C-k               | kill-line-backwards
C-x _tab_               | Enter indentation mode (sometimes need C-u)
{C,M}-t                 | Jump {forwards, backwards} to a character
C-c C-{f,b}             | HTML-mode: Go to matching tag
M-z _char_              | Delete until _char_

##emacs + python
- C-M-a
- C-M-e
- M-a
- M-e
- C-M-f
- C-M-b
- **toggle comment:** <select region> M-;

##python
- **postmortem debugger:** `python3 -m pdb foo.py`
  - **stack:** `where`, `up`, `down`
  - **run code:**: `!`, `p`, `pp`, `interact`
  - **context:** `list`, `list start, end`, `args`

##encryption
```
tar -cz foo | openssl aes-256-cbc > foo.tar.gz.a2c
mkdir -p foo2 && cat foo.tar.gz.a2c | openssl aes-256-cbc -d | tar -xz -C foo2

export GPG_TTY=$(tty) && tar -cz foo | gpg -c > foo.tar.gz.gpg
mkdir -p foo2 && cat foo.tar.gz.gpg | gpg -d | tar -xz -C foo2

zip -e -r foo foo
unzip -d foo2 foo.zip
```

##vid -> gif
ffmpeg -i IMG_1246.m4v -s 360x640 -pix_fmt rgb24 -r 10  -f gif - | gifsicle --optimize=3 --delay=10 > out.gif

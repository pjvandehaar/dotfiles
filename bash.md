##network
- `lsof -i`
- `sshfs user@host:dir mountpoint`
- `netstat -rn | grep default`

##bash
- `type ls`  
- `diff <(sort .bashrc) <(sort .bash_profile)`
- `history | tail -n1000 | sort -k2 | less`
  - requires `export HISTTIMEFORMAT="%Y/%m/%d %T"`
- **expand inline:** C-A-e, C-x *
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
- **selectively revert changes:** `git checkout -p`
- `git grep -E 'pattern'`

##csv
- `pip3 install csvkit`
- `csvcut -c bar,baz foo.csv`

##emacs
key                     | effect
------------------------|-------
M-x `menu-bar-open`, F10| 
C-h k                   | get help on key
_region_ `M-|` `pbcopy` | copy region to mac clipboard
C-x {2,3}               | view current buffer in two "windows"
C-x 1                   | close all other "windows"
C-x 0                   | close this "window"
C-x C-f                 | view a file
M->                     | end of buffer
M-^                     | join previous line
M-m                     | move to first character on line
M-g g _linenum_         |
C-M-{n,p}               | matching paren
M-l                     | move window around cursor
M-r                     | move cursor to center of window
C-q <tab>               |


##emacs + python
- C-M-a
- C-M-e
- M-a
- M-e
- C-M-f
- C-M-b

##python
- **postmortem debugger:** `python3 -m pdb foo.py`
  - **stack:** `where`, `up`, `down`
  - **run code:**: `!`, `p`, `pp`, `interact`
  - **context:** `list`, `list start, end`, `args`
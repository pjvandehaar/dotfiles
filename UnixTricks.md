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

##git
- **use colors to combine add/remove lines:** `git diff --color-words`
- **characterwise diff:** `git diff --color-words=.`
- **selectively revert changes:** `git checkout -p`

##homebrew
- `brew leaves`
- `brew {cask,} {doctor,cleanup}`

##pip
- **show leaves:** `pip3 install pip-autoremove && pip-autoremove -L`

##csv
- `pip3 install csvkit`
- `csvcut -c bar,baz foo.csv`

##iTerm2
- **split:** Command-D
- **switch:** Command-option-arrow
- **dump scrollback:** Command-K

##emacs
key                     | effect
------------------------|-------
M-x `menu-bar-open`, F10| 
C-h k                   | get help on key
_region_ M-\| `pbcopy`  | copy region to mac clipboard
C-x {2,3}               | view current buffer in two "windows"
C-x 1                   | close all other "windows"
C-x 0                   | close this "window"
C-x C-f                 | view a file
M->                     | end of buffer
M-^                     | join previous line
M-m                     | move to first character on line
M-g g _num_             | goto line
C-M-{n,p}               | matching paren


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

##OSX troubleshooting
- **wifi:** `rm /L*/Pr*ces/S*/{com.apple.{air,net},Net,pref}*.plist`
- **reset SMC:** hold _C-M-S-powerbutton_ for 3 seconds while the computer is off but plugged in.
  + do this for issues related to: MagSafe, battery, fans, power button, lid, unexpected shutdown/sleep.
  + expect MagSafe light to change.
- **reset NVRAM/PRAM:**: boot with _Command-Option-P-R_ until restart and second chime.
  + do this for issues related to: speaker volume, screen resolution, startup disk selection, recent kernel panic information.
- **Recovery Mode:** boot with _Command-R_ or _Command-Option-R_ (via internet).
  + use this to reinstall OSX or fix/verify drives.
- **Safe Mode:**: boot with _Shift_.
  + use this if the computer crashes during bootup.  It will fix the recovery/boot partition.
- **Diagnotics Mode:**: boot with _D_ or _Option-D_ (via internet).
- **Verbose Mode:**: boot with _Command-V_.
- **Single-User Mode:**: boot with _Command-S_.

Monitoring with terminal commands:

- **monitor file openings:** `sudo opensnoop -ve | egrep "(UID|$UID)"`
- **check who's taking all the disk io:** `sudo iosnoop -stoD`
- **monitor starting processes:** `sudo execsnoop -v`

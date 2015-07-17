##network
- `lsof -i`
- `sshfs user@host:dir mountpoint`
- `netstat -rn | grep default`

##bash
- `type ls`  
- `diff <(sort .bashrc) <(sort .bash_profile)`
- `history | tail -n1000 | sort -k2 | less`
	- requires `export HISTTIMEFORMAT="%Y/%m/%d %T"`
- **expand inline:** C-A-e
- `echo $OLDPWD`

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
- **split:** Command-d
- **switch:** Command-option-arrow

##emacs
key                    | effect
-----------------------|-------
M-x menu-bar-open, F10 | 
C-h k                  | get help on key
_region_ M-\| pbcopy   | copy region to mac clipboard
C-x {2,3}              | view current buffer in two "windows"
C-x 1                  | close all other "windows"
C-x 0                  | close this "window"
C-x C-f                | view a file
M->                    | end of buffer
M-^                    | join previous line
M-m                    | move to first character on line
M-g g _num_            | goto line

##emacs + python
- C-M-a
- C-M-e
- M-a
- M-e
- C-M-f
- C-M-b

##python
- **postmortem debugger:** `python3 -m pdb foo.py`

##OSX troubleshooting
- **wifi:** `rm /L*/Pr*ces/S*/{com.apple.{air,net},Net,pref}*.plist`
- **power:** off, power cable, C-M-S-power_button for 3 seconds, boot
- **corruption:** boot with either {option} or {Command-R}. DiskUtility->Repair
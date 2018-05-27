## OSX troubleshooting
- **wifi:**
  + `rm /L*/Pr*ces/S*/{com.apple.{air,net},Net,pref}*.plist`
  + shut off bluetooth
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

## Monitoring with terminal commands:

- **monitor file openings:** `sudo opensnoop -ve | egrep "(UID|$UID)"`
- **check who's taking all the disk io:** `sudo iosnoop -stoD`
- **monitor starting processes:** `sudo execsnoop -v`

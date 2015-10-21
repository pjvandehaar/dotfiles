- **Disable spelling correction:** System Preferences > Keyboard > Text > (uncheck) Correct Spelling Automatically
- **Disable Transparency:** System Preferences > Accessibilty > Display > Reduce Transparency
- **Disable Space Rearrangement:** System Preferences > Mission Control > (uncheck) Automatically rearrange Spaces based on most recent use
- **Add Korean Keyboard**
	- System Preferences > Language & Region > Preferred Languages > [+] > Korean > 2-set
	- System Preferences > Keyboard > Shortcuts > Input Sources > Select Next Input Source > opt-com-space
	- System Preferences > Keyboard > Keyboard > Show Keyboard & Character Viewers in menubar
- **Shut off Bluetooth:** System Preferences > Bluetooth > Turn Bluetooth off
- **Change Name on Network:** System Preferences > Sharing > Computer Name > (type)
- **Enable three-finger dictionary:** System Preferences > Trackpad > Point & Click > Look up & data detectors > Tap with three fingers
- **Expand save panel by default:**

        defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
        defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

- **Allow tab in modals:** `defaults write NSGlobalDomain AppleKeyboardUIMode -int 3`

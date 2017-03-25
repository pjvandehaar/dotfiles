#!/bin/bash

# Disable spelling correction (System Preferences > Keyboard > Text > (uncheck) Correct Spelling Automatically)
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# Disable Transparency (System Preferences > Accessibilty > Display > Reduce Transparency)
defaults write com.apple.universalaccess reduceTransparency -bool true

# Disable Space Rearrangement (System Preferences > Mission Control > (uncheck) Automatically rearrange Spaces based on most recent use)
defaults write com.apple.dock mru-spaces -bool false

# Add Korean Keyboard
# - System Preferences > Language & Region > Preferred Languages > [+] > Korean > 2-set
# - System Preferences > Keyboard > Shortcuts > Input Sources > Select Next Input Source > opt-com-space
# - System Preferences > Keyboard > Keyboard > Show Keyboard & Character Viewers in menubar

# Change Name on Network
# - System Preferences > Sharing > Computer Name > (type)

# Expand save panel by default:
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Allow tab in modals
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

# Show hidden files
defaults write com.apple.finder AppleShowAllFiles TRUE
killall Finder
chflags nohidden ~/Library
sudo chflags nohidden /Volumes

# maybe use this to improve bluetooth headphone sound quality? see <http://apple.stackexchange.com/a/105952/137251>
#defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 53


### TRACKPAD
# - tap to click
# - click: light
# - tracking speed: medium fast
# - silent click
# - swipe between pages with 3 fingers
# - swipe between "full-screen apps" (spaces) with 4 fingers



#### FINDER
# Show file extensions (Preferences > Advanced > Show all filename extensions)
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Show path in window title
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

# Hide path bar at bottom
defaults write com.apple.finder ShowPathbar -bool false

# Default to list view (Others: `icnv`, `clmv`, `Flwv`)
defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"

# disable window animations and Get Info animations
defaults write com.apple.finder DisableAllAnimations -bool true

# no .DS_Store on network devices
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Wipe all (default) app icons from the Dock
# This is only really useful when setting up a new Mac, or if you don’t use
# the Dock to launch apps.
#defaults write com.apple.dock persistent-apps -array



#### PHOTOS
# Prevent Photos from opening automatically when devices are plugged in
defaults -currentHost write com.apple.ImageCapture disableHotPlug -bool true



#### TRANSMISSION
# Trash original torrent files
defaults write org.m0k.transmission DeleteOriginalTorrent -bool true

# Hide the donate message and legal disclaimer
defaults write org.m0k.transmission WarningDonate -bool false
defaults write org.m0k.transmission WarningLegal -bool false



#### references:
# <https://github.com/aanari/dotfiles/blob/master/macos.sh>

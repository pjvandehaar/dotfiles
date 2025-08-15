#!/bin/bash

# Disable spelling correction (System Preferences > Keyboard > Text > (uncheck) Correct Spelling Automatically)
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

# Disable Space Rearrangement (System Preferences > Mission Control > (uncheck) Automatically rearrange Spaces based on most recent use)
defaults write com.apple.dock mru-spaces -bool false

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



#### SAFARI
# Download to Desktop (Prefs > General > File download location = ~/Desktop)




#### references:
# <https://github.com/aanari/dotfiles/blob/master/macos.sh>

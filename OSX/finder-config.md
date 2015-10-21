- Show hidden files

        defaults write com.apple.finder AppleShowAllFiles TRUE
        killall Finder

- Show file extensions
  - Preferences > Advanced > Show all filename extensions
- Show path in window title: `defaults write com.apple.finder _FXShowPosixPathInTitle -bool true`
- Default to list view: `defaults write com.apple.finder FXPreferredViewStyle -string "Nlsv"`
  - Others: `icnv`, `clmv`, `Flwv`

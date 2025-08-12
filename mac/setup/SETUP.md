## How to setup a new Macbook

- Empty dock, hide

- Install homebrew, then `brew install bash emacs uv tig htop mosh wget zstd jq alfred hyperkey rectangle-pro macdown spotify vlc wireshark-app visual-studio-code`

- HyperKey:
  - Remap physical key to hyperkey = CapsLock
  - Apply hyperkey to [Click Drag Move]
  - Open on login
  - Check for updates automatically

- RectanglePro:
  - General > Purchase
    - search gmail for "Rectangle Pro order"
  - General > Sync config over iCloud
    - hopefully this imports everything below
  - Window Throw > Configure > 3/4, halves, 1/4, center 1/2
  - Window Throw > Modifer Key = CapsLock (via HyperKey)
  - Window Throw > 3sliders > Target = Ring
  - Keyboard Shortcuts > disable all
  - General > Check for updates automatically
  - General > Doubleclick window title bar to maximize/restore

- iTerm: see [./iterm-config.md]

- python: `uv venv ~/venv/`, new terminal, `uv pip install kpa requests unidecode boltons ipython jupyter numpy pandas psutil tqdm twine visidata`

- Alfred: see [./alfred-config.md]

- Mac: see [./SystemPreferences-config.sh]
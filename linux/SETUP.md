## How to setup a new Ubuntu Server

- If this is a VPS, consider following `./setup-VPS.md`.

- `git clone https://github.com/pjvandehaar/dotfiles.git && ~/dotfiles/install.py`

- `sudo apt install emacs htop tmux curl mosh tig jq`
  - If emacs is slow: start emacs service/server and make sure `e` uses it.  Try `emacs -Q` (no conf).

- Install uv (using `curl | sh`)
  - `uv venv ~/venv`
  - Setup .bashrc to activate that, or add to PATH, or however.
  - `uv pip install kpa requests psutil unidecode boltons ipython jupyter numpy pandas tqdm twine visidata`

- `mkdir ~/PROJECTS/`

- Consider installing `~/PROJECTS/kpa/`.

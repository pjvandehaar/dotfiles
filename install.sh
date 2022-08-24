#!/usr/bin/env bash
set -euo pipefail
readlinkf() { perl -MCwd -le 'print Cwd::abs_path shift' "$1"; }
dotfiles_path="$(cd "$(dirname "$(readlinkf "${BASH_SOURCE[0]}")")" && echo "$PWD")"
python3 "$dotfiles_path/install.py"

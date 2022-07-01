#!/usr/bin/env bash
set -euo pipefail
readlinkf() { perl -MCwd -le 'print Cwd::abs_path shift' "$1"; }

# TODO: rewrite in python to allow `symlinks/~%.config%htop%htoprc` and to show all conflicts at once.


linkify_directory() {
    dir="$(readlinkf "$1")"
    filepaths_array=();
    while IFS=  read -r -d $'\0'; do filepaths_array+=("$REPLY"); done < <(find "$dir" -type f ! -name '*~*' -print0)
    for targetpath in "${filepaths_array[@]}"; do
        linkpath="$HOME/$(basename "$targetpath")"
        if [[ -L "$linkpath" ]]; then
            current_target="$(readlinkf "$linkpath")"
            if [[ "$current_target" != "$targetpath" ]]; then
                echo "bad: [$linkpath] -> [$current_target] but should be [$targetpath]"
                ln -si "$targetpath" "$linkpath"
            else
                echo "good: [$linkpath] -> [$targetpath]"
            fi
        elif ! [[ -e "$linkpath" ]]; then
            echo "creating [$linkpath] -> [$targetpath]"
            ln -s "$targetpath" "$linkpath"
        else
            echo "bad (not a link): [$linkpath]"
            exit 1
        fi
    done
}

dotfiles_path="$(cd "$(dirname "$(readlinkf "${BASH_SOURCE[0]}")")" && echo "$PWD")"
echo dotfiles_path = "$dotfiles_path"
linkify_directory "$dotfiles_path/symlinks"

if [[ "$OSTYPE" == darwin* ]]; then
    echo mac
    linkify_directory "$dotfiles_path/mac/symlinks"
elif [[ "$OSTYPE" == linux-gnu ]]; then
    echo linux
    linkify_directory "$dotfiles_path/linux/symlinks"
else
    echo unknown
    exit 1
fi

echo SUCCESS

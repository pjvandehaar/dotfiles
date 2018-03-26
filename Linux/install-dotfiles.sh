set -euo pipefail

cd "$HOME"
for filename in .inputrc .emacs .hgrc .gitignore_global .tigrc .Rprofile .byoburc.tmux .tmux.conf Linux/.gitconfig Linux/.bash_profile Linux/.bashrc; do
    linkfile="$HOME/$(basename $filename)"
    targetfile="$HOME/dotfiles/$filename"
    if [[ ! -e "$linkfile" ]]; then
        echo "creating link [$linkfile] -> [$targetfile]"
        ln -s "$targetfile" "$linkfile"
    elif [[ -L "$linkfile" ]]; then
        current_link_target="$(readlink -f "$(\ls -l "$linkfile" | sed 's#.* -> ##')")"
        if [[ "$current_link_target" != "$targetfile" ]]; then
            echo the file "[$linkfile]" is currently pointing to "[$current_link_target]" instead of "[$targetfile]"
            ln -si "$targetfile" "$linkfile"
        fi
    else
        echo file exists but is not a link: "$linkfile"
    fi
done

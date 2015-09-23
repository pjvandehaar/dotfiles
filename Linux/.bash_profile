# there's no danger in double-including .bashrc, because it tests whether `arb` is a function.
type -t arb || . .bashrc

# frodo runs screen to reach flux.
if [[ $HOSTNAME != frodo ]]; then
    _byobu_sourced=1 . /usr/bin/byobu-launch
fi

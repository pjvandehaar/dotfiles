#!/bin/bash
{
set -euo pipefail
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

BUILDDIR="$SCRIPTDIR/resources-tmux/build"
mkdir -p "$BUILDDIR"
cd "$BUILDDIR/.."

echo '############'
echo '# libevent #'
echo '############'
wget -O "libevent.tar.gz" https://github.com/libevent/libevent/releases/download/release-2.0.22-stable/libevent-2.0.22-stable.tar.gz
tar -xzf "libevent.tar.gz"
cd libevent-*
./configure --prefix="$BUILDDIR" --disable-shared
make
make install
cd ..

echo '############'
echo '# ncurses  #'
echo '############'
wget -O "ncurses.tar.gz" ftp://ftp.gnu.org/gnu/ncurses/ncurses-6.0.tar.gz # Can tmux handle ncurses6? I see ncurses5 most places.
# wget -O "ncurses.tar.gz" ftp://ftp.gnu.org/gnu/ncurses/ncurses-5.9.tar.gz
tar -xzf "ncurses.tar.gz"
cd ncurses-*
./configure --prefix="$BUILDDIR"
make
make install
cd ..

echo '############'
echo '# tmux     #'
echo '############'
wget -O "tmux.tar.gz" https://github.com/tmux/tmux/releases/download/2.2/tmux-2.2.tar.gz
tar xzf "tmux.tar.gz"
cd tmux-*
./configure CFLAGS="-I$BUILDDIR/include -I$BUILDDIR/include/ncurses" LDFLAGS="-L$BUILDDIR/lib -L$BUILDDIR/include/ncurses -L$BUILDDIR/include"
CPPFLAGS="-I$BUILDDIR/include -I$BUILDDIR/include/ncurses" LDFLAGS="-static -L$BUILDDIR/include -L$BUILDDIR/include/ncurses -L$BUILDDIR/lib" make

mkdir -p $HOME/bin/
cp tmux $HOME/bin/
cd ..

echo '#########'
"$HOME/bin/tmux" -V
echo "Strings that snuck into the binary:"
strings "$HOME/bin/tmux" | grep "$SCRIPTDIR"
}

#!/bin/bash

{
set -euo pipefail

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p "$SCRIPTDIR/dl"

wget -O - https://github.com/git/git/archive/v2.9.0.tar.gz |
tar -xz -C "$SCRIPTDIR/dl"

cd "$SCRIPTDIR/dl/git-"*

make configure

INSTALLDIR="$SCRIPTDIR/git-install-dir"
mkdir -p "$INSTALLDIR"
./configure --prefix="$INSTALLDIR"

make
make install

cp "$INSTALLDIR/bin/git" ~/bin/git

cd "$SCRIPTDIR"

rm -rf "$SCRIPTDIR/dl" "$INSTALLDIR"
}

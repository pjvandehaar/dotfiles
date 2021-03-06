#!/bin/bash

{
set -euo pipefail

echo "THIS INSTALLS GIT WITHOUT OPENSSL, SO YOU CAN'T USE HTTPS.  THAT REALLY SUCKS.  LET'S EITHER FIX THIS OR REMOVE IT IN FAVOR OF LINUXBREW"

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BUILDDIR="$SCRIPTDIR/resources-git"

mkdir -p "$BUILDDIR"

cd "$BUILDDIR"
wget -O - https://github.com/git/git/archive/v2.9.0.tar.gz |
tar -xz

cd "$BUILDDIR/git-"*

make configure

INSTALLDIR="$BUILDDIR/install"
mkdir -p "$INSTALLDIR"
./configure --prefix="$INSTALLDIR"

make
make install

mkdir -p ~/bin
cp "$INSTALLDIR/bin/git" ~/bin/git

cd "$SCRIPTDIR"

rm -rf "$SCRIPTDIR/git-*"
}

# OSX won't source .bashrc unless told to.
type -t cutdammit || . "$(dirname $(greadlink -f ${BASH_SOURCE[0]}))/.bashrc"

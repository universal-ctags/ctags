# Copyright: 2019 itchyny
# License: GPL-2

CTAGS=$1

. ../utils.sh

# $XDG_CONFIG_HOME/ctags/*.ctags are loaded
export XDG_CONFIG_HOME=./.config
${CTAGS}

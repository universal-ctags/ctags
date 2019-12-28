# Copyright: 2019 itchyny
# License: GPL-2

CTAGS=$1

. ../utils.sh

exit_if_win32 "$CTAGS"

# $HOME/.config/ctags/*.ctags are loaded
export HOME=./myhome
export XDG_CONFIG_HOME=
${CTAGS}

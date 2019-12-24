# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

exit_if_win32 "$CTAGS"

export XDG_CONFIG_HOME=./.config
${CTAGS}

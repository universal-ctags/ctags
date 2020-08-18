# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

exit_if_win32 "$CTAGS"
skip_if_user_has_dot_ctags_d

${CTAGS}

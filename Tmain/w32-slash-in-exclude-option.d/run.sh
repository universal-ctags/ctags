# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

exit_unless_win32 "$CTAGS"

MSYS2_ARG_CONV_EXCL=dont_capture_me/input.c ${CTAGS} --quiet --options=NONE -R -o - \
		 --exclude='input.d/dont_capture_me/input.c' input.d

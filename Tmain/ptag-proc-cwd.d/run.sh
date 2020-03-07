# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
exit_if_win32 $CTAGS

O="--quiet --options=NONE "

(
	cd /
	${CTAGS} $O \
			 --extras=+p --pseudo-tags=TAG_PROC_CWD \
			 -o - \
			 input.c
)
